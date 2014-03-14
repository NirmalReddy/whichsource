package controllers

import akka.actor.ActorRef
import akka.actor.Props
import akka.pattern.ask
import akka.util.Timeout
import java.io._
import models.Indexing._
import models._
import play.api.Play.current
import play.api.data.Forms._
import play.api.data._
import play.api.libs.concurrent.Akka
import play.api.libs.concurrent.Execution.Implicits._
import play.api.mvc._
import play.api.templates.Html
import scala.concurrent._
import scala.concurrent.duration._
import org.eclipse.jdt.core.dom._

object Application extends Controller {
  implicit val timeout = Timeout(6.second)

  val traceForm = Form(
    tuple(
      "trace" -> text,
      "submit" -> ignored(AnyRef)))

  def index = Action {
    main()
    Ok(views.html.index("Paste your stacktrace here."))
  }

  def sourceFinder = Action.async { implicit request =>
    traceForm.bindFromRequest().fold(
      errors => Future(BadRequest("Not a valid stacktrace.")),
      trace => {
        val (t, _) = trace
        gatherTraceInfo(t)
      })
  }

  def gatherTraceInfo(trace: String): Future[SimpleResult] = {
    val askers = spawnAskers(trace)
    if (askers.isEmpty) {
      Future(BadRequest("Not a valid stacktrace."))
    } else {
      val resMap = askAndAcc(askers)
      resMap map { x: List[(String, Html)] =>
        processStack(trace, x)
      }
    }
  }

  def spawnAskers(t: String) = {
    val funcLines = t.split('\n').filter(_.contains("java:")).toList

    val funcs = funcLines map { x =>
      val s = x.split(' ').last.split('(')
      val f = s.head
      val l = s.last.split(':').last.split(')').head
      (f, l)
    }

    val fqns = funcs map (p => p._1)

    val pkgPrefixes = fqns map { x =>
      x.split('.').takeWhile(_.charAt(0).isLower).mkString(".")
    }

    val prefixSet = pkgPrefixes.toSet

    val groups = prefixSet map { x =>
      funcs.filter { case (k, v) => k.contains(x) }
    }

    val askers = groups map { g =>
      (Akka.system.actorOf(Props[Asker]), SearchFuncs(g))
    }
    askers
  }

  def askAndAcc(askers: Set[(ActorRef, SearchFuncs)]) = {
    val futures = askers.map(a => a._1 ? a._2)
    val names = Future.fold(futures)(List.empty[(String, Html)])((a, b) => b match {
      case GotIt(funcs) => a ++ b.asInstanceOf[GotIt].funcs
    })
    names
  }

  def processStack(trace: String, fMap: List[(String, Html)]) = {
    val linenos = for(result <- fMap) yield result._1.split(':').last
    val h = Html(trace.split('\n').map { x =>
      if (x.contains(".java:")) {
        val line = x.split(':').last.split(')').head
        "<li><p title='" + x + "' class='traceLine' id='func" + line + "' onclick='showSource(" + line + ")'>" +
          shortHand(x) + "</p></li>"
      } else {
        x
      }
    }.mkString(" "))
    Ok(views.html.results(fMap.zip(linenos), h))
  }

  def shortHand(s: String): String = {
    val list = s.split(' ').last.split('.').zipWithIndex
    list.map { x =>
      if (x._2 > 4 && x._2 < (list.size - 3)) "."
      else if (x._2 < (list.size - 3))        "" + x._1.head
      else                                    x._1
    }.mkString(".")
  }

  def getFunc(f: String) = Action {
    val fName = f.split('&')(0)
    Functions.getFunc(fName).map { func =>
      Ok(views.html.function(func.map{ f =>
        views.html.source.render(f.jarName + " inside " + f.name.split('.').dropRight(1).last +".java ", f.body.map(g => g.split('\n').toList).getOrElse(List()), f.start, 0)
        }.toList, Html("<p class='traceLine'>" + fName + "</p>")))
    }.getOrElse(Ok(views.html.function(List(Html("This function is not available.")), Html("<p>" + fName + "</p>"))))
  }

  def uploadJar = Action(parse.multipartFormData) { request =>
    request.body.file("jar").map { jar =>
      val filename = jar.filename
      jar.ref.moveTo(new File(s"jars/$filename"))
      MapIndexer.index(s"jars/$filename")
      Ok("Jar uploaded")
    }.getOrElse {
      Redirect(routes.Application.index).flashing(
        "error" -> "Missing file")
    }
  }

  def main() {
    val parser: ASTParser = ASTParser.newParser(AST.JLS4)
    parser.setSource("public class A { int i = 9;  \n int j; \n ArrayList<Integer> al = new ArrayList<Integer>();j=1000; }".toCharArray)
    parser.setKind(ASTParser.K_COMPILATION_UNIT)
    val cu: CompilationUnit  = parser.createAST(null).asInstanceOf[CompilationUnit]
    cu.accept(new ASTVisitor() {
      var names = List[String]()

      override def visit(node: VariableDeclarationFragment): Boolean = {
        val name = node.getName
        names ::= name.getIdentifier
        println("Declaration of '"+name+"' at line"+cu.getLineNumber(name.getStartPosition))
        false
      }

      override def visit(node: SimpleName): Boolean = {
        println(node.getIdentifier)
        if (this.names.contains(node.getIdentifier)) {
          println("Usage of '" + node + "' at line " + cu.getLineNumber(node.getStartPosition))
        }
        true
      }
    })
  }
}