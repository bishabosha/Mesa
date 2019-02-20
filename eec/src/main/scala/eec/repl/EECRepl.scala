package eec
package repl

class EECRepl {
  
  import compiler._
  import types._
  import parsing._
  import Namers._
  import Types._
  import core.Contexts._
  import error.CompilerErrors._
  import scala.annotation._
  import pprint._

  val pwd = System.getProperty("user.dir")
  val defaultPrompt = "eec"

  def loop: Unit = {

    def (prompt: String) asPrompt: String = s"$prompt> "

    @tailrec
    def inner(state: LoopState): Unit = state match {
      case s @ LoopState(prompt, false) =>
        println
        print(prompt.asPrompt)
        val nextState = command(s, readLine)
        inner(nextState)
      case LoopState(_, true) =>
        // exit
    }

    val initial = LoopState(defaultPrompt, false)
    println("starting eec REPL...")
    print(defaultPrompt.asPrompt)
    val state = command(initial, readLine)
    inner(state)
  }

  private[this] case class LoopState(prompt: String, break: Boolean)

  private[this] def loadFile(name: String): Checked[String] = {
    import scala.util.control._
    var file: scala.io.BufferedSource = null
    try {
      file = scala.io.Source.fromFile(s"$pwd/$name")
      return file.getLines.mkString("\n")
    } catch {
      case e: Exception if NonFatal(e) =>
        CompilerError.IllegalState(e.getMessage)
    } finally {
      if file ne null then {
        file.close()
      }
    }
  }

  private[this] def command(state: LoopState, input: String): LoopState = {

      import Commands._
      import Command._
      import ast._
      import Typers._
      import core.Printing.untyped.AstOps._
      import CompilerErrorOps._

      def guarded(string: String)(body: => LoopState): LoopState =
        if string.isEmpty then {
          println(s"[ERROR] empty input")
          state
        } else {
          body
        }

      parseCommand(input) match {
        case AstExpr(code) => guarded(code) {
          implicit val rootCtx = new RootContext()
          parseExpr(code).fold
            { err => println(s"[ERROR] ${err.userString}") }
            { expr => pprintln(expr.toAst, height = Int.MaxValue) }

          state
        }
        case TypeExpr(code) => guarded(code) {
          import TypeOps._
          import ContextOps._
          implicit val rootCtx = new RootContext()
          val yieldTyped = for {
            _     <- Context.enterBootstrapped
            expr  <- parseExpr(code)
            _     <- indexAsExpr(expr).recoverDefault
            typed <- expr.typedAsExpr(Type.WildcardType)
          } yield typed

          yieldTyped.fold
            { error => println(s"[ERROR] ${error.userString}") }
            { typed =>
              println(typed.tpe.userString)
              // pprintln(rootCtx.toScoping, height = Int.MaxValue)
            }

          state
        }
        case AstFile(name) => guarded(name) {
          import ContextOps._
          implicit val rootCtx = new RootContext()
          val yieldAst = for {
            _     <- Context.enterBootstrapped
            code  <- loadFile(name)
            ast   <- parseEEC(code)
          } yield for {
            _ <- indexAsExpr(ast).recoverDefault
          } yield ast

          yieldAst.fold
            { err => println(s"[ERROR] ${err.userString}") }
            { ast =>
              pprintln(ast.toAst, height = Int.MaxValue)
              // pprintln(rootCtx.toScoping, height = Int.MaxValue)
            }

          state
        }
        case SetPrompt(newPrompt) => guarded(newPrompt) {
          state.copy(prompt = newPrompt)
        }
        case Quit =>
          println("Quitting...")
          state.copy(break = true)
        case ShowHelp =>
          println(helpText)
          state
        case Unknown =>
          println(s"[ERROR] unrecognised command: `$input`. Try `:help`")
          state
      }
    }
}