package models

import Indexing._
import akka.actor._
import com.typesafe.config.ConfigFactory
import java.io.{File, InputStream, FileWriter, FileNotFoundException}
import java.util.jar.JarFile
import org.eclipse.jdt.core.dom._
import play.api.templates.Html
import play.libs.Akka
import scala.collection.JavaConversions._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent._
import scala.io.Codec


object Indexing {
  case class GotIt(funcs: List[(String, Html)])
  case class SearchFuncs(cond: List[(String, String)])
  case class DoneIndexing(jarPath: String)
  case class NotIt(fl: List[String])
  case class Index(jarPath: String)
}

class Ind extends Actor {
  def receive = {
    case x => val fw = new FileWriter(new File("log"), true)
      try {
        fw.append(x + "\n")
        fw.flush()
      } finally {
        fw.close()
      }
  }
}

trait IndexerService {
  val ind = Akka.system.actorOf(Props[Ind])
  def jarDir: String
  def jars = new java.io.File(jarDir).listFiles().filter(_.getName().endsWith(".jar"))
  var jarsToIndex: Int
  def jarListBackup: String
  def cachedJars = new java.io.File(jarListBackup)

  private val indexer = Akka.system.actorOf(Props(new Actor {
    def receive = {
      case DoneIndexing(jarPath) =>
        ind ! s"Jars left were $jarsToIndex"
        jarsToIndex -= 1
        ind ! s"Jars now left $jarsToIndex"
        indexingFinished(jarPath)
        if (jarsToIndex == 0) {
          ind ! "Indexing finished."
          persistIndex
        }

      case Index(jarPath: String) =>
        future(index(jarPath))
    }
  }), "indexer")

  def addFunc(fName: String, f: Func): Unit

  def persistIndex: Unit

  def index(jarPath: String) {
    val jarFile = new JarFile(jarPath)
    ind ! s"Indexing $jarPath now."
    jarFile.entries.filter(_.getName.contains(".java")) foreach { x =>
      val nm = x.getName
      ind ! s"Inside $nm"
      extractMethods(x.getName, jarFile.getInputStream(x), jarPath)
    }
    ind ! "finished with this jar"
    indexer ! DoneIndexing(jarPath)
  }

  /**
   * Extracting Methods From each .java entry in the JAR.
   *
   * @param fName *.java name
   * @param is JAR as inputstream
   * @param jarPath JAR path
   */
  def extractMethods(fName: String, is: InputStream, jarPath: String) {
    val parser = ASTParser.newParser(AST.JLS4)
    // or use codec "latin1" !!!important
    parser.setSource(io.Source.fromInputStream(is)(Codec.ISO8859).toSeq.toArray)
    parser.setKind(ASTParser.K_COMPILATION_UNIT)
    parser.setEnvironment(null, Array(jarPath), null, true)
    parser.setUnitName("indexer")
    parser.setResolveBindings(true)
    val cu = parser.createAST(null).asInstanceOf[CompilationUnit]
    val fqn = cu.getPackage.getName + "." + fName.split('/').last.split('.').head
    cu.accept(new MethodVisitor(cu, fqn, jarPath))
  }


  class MethodVisitor(cu: CompilationUnit, fqn: String, jarName: String) extends ASTVisitor {
    override def visit(node: MethodDeclaration) = {
      val start = cu.getLineNumber(node.getStartPosition)
      val end = cu.getLineNumber(node.getStartPosition + node.getLength)
      val body = processBody(node)
      val fullyQualifiedName = getFullName(node)
      val index = fullyQualifiedName + "(" + node.parameters().map { x =>
        x.toString.split(" ").head
      }.mkString(",") + "):" + node.getReturnType2
      addFunc(index, new Func(fullyQualifiedName, start, end, body, jarName.split('/').toList.last))
      super.visit(node)
    }

    def getFullName(node: MethodDeclaration): String = {
      Option(node.resolveBinding()).map { x =>
        if(fqn.equals(x.getDeclaringClass.getPackage.getName + "." + x.getDeclaringClass.getName))
          fqn + "." + x.getName
        else
          fqn + "." + x.getDeclaringClass.getName + "." + x.getName
      }.getOrElse(fqn + "." + node.getName.getFullyQualifiedName)
    }

    /**
     * Processing the body and allowing to navigate through all the method calls.
     *
     * @param node
     * @return
     */
    def processBody(node: MethodDeclaration): Option[String] = {
      Option(node.getBody) match {
        case Some(x) =>
          val invokeMap = scala.collection.mutable.Map.empty[String, String]
          Option(node.getBody).map(_.accept(new MethodInvocationVisitor(invokeMap)))
          var body = node.toString.replaceAll("&", "&amp;").replace(">", "&gt;").
            replace("<", "&lt;").replace("\"", "&quot;")
          invokeMap foreach {
            case (k, v) =>
              body = body.replaceAll(k + """\(""", "<a href='/func/" +
                v.split('&').toList.head + "' title = '" +
                v.split('&').toList.last + "'>" + k + "</a>(")
          }
          Some(body)

        case None => Some(node.toString)
      }

    }

    class MethodInvocationVisitor(invokeMap: scala.collection.mutable.Map[String, String]) extends ASTVisitor {
      override def visit(node: MethodInvocation) = {
        val name = node.getName.getFullyQualifiedName
        val specialChars = Array[Char]('[',']')
        invokeMap += (name -> Option(node.resolveMethodBinding()).map { x =>
          x.getDeclaringClass.getPackage.getName + "." + x.getDeclaringClass.getName + "." +
            name + "%28" + x.getParameterTypes.map { x =>
              x.getName.map { y =>
              if(specialChars.contains(y)) '%' + Integer.toHexString(y.toInt) else y
            }.mkString
          }.mkString(",") + "%29:" + x.getReturnType.getName.map { y =>
            if(specialChars.contains(y)) '%' + Integer.toHexString(y.toInt) else y
          }.mkString + "&" + x.getDeclaringClass.getPackage.getName + "." +
            x.getDeclaringClass.getName + "." + name + "(" + x.getParameterTypes.map { x => x.getName
          }.mkString(",") + "):" + x.getReturnType.getName
        }.getOrElse(""))
        super.visit(node)
      }
    }
  }


  def indexingFinished(jar: String) {
    val fw = new FileWriter(new File(jarListBackup), true)
    try {
      fw.append(jar + "\n")
      fw.flush()
    } finally {
      fw.close()
    }
  }

}

object MapIndexer extends IndexerService {
  val jarDir        = ConfigFactory.load.getString("repoPath")
  val jarListBackup = ConfigFactory.load.getString("jarListBackup")
  var jarsToIndex   = jars.length

  def addFunc(fName: String, f: Func) {
    Functions.getFunc(fName) match {
      case Some(x) => if (x.isEmpty) Functions.add(fName, f)
      case None => Functions.add(fName, f)
    }
  }

  def persistIndex = Functions.store

  def init {
    Functions.load
    try {
      val files = io.Source.fromFile(cachedJars).getLines
      jars foreach { x: java.io.File =>
        if (!files.contains(jarDir + x.getName)) {
          println(jarDir + x.getName)
          future((index(x.getPath)))
        } else {
          jarsToIndex -= 1
        }
      }
    } catch {
      case e: FileNotFoundException =>
        jars foreach(x => future((index(x.getPath))))
    }
  }
}