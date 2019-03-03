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
  import util.Convert
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
      import Typers._
      import ast.Trees._
      import core.Printing.untyped.AstOps._
      import CompilerErrorOps._
      import implied CompilerErrorOps._

      def Typed(s: String)(f: String => Contextual[Checked[Tree]]): (
        LoopState) = guarded(s) {
          import implied TypeOps._
          import ContextOps._
          val rootCtx = new RootContext()
          implied for Context = rootCtx
          val yieldTyped = for {
            _     <- Context.enterBootstrapped
            expr  <- f(s)
            _     <- indexAsExpr(expr).recoverDefault
            typed <- expr.typedAsExpr(Type.WildcardType)
          } yield typed

          yieldTyped.fold
            { error => println(s"[ERROR] ${error.userString}") }
            { typed =>
              println(typed.tpe.userString)
              pprintln(rootCtx.toScoping, height = Int.MaxValue)
            }

          state
        }

      def Ast(s: String)(f: String => Contextual[Checked[Tree]]): (
        LoopState) = guarded(s) {
          import TypeOps._
          import ContextOps._
          val rootCtx = new RootContext()
          implied for Context = rootCtx

          val yieldNamed = for {
            _     <- Context.enterBootstrapped
            expr  <- f(s)
            _     <- indexAsExpr(expr).recoverDefault
            ex    <- expr
          } yield ex

          yieldNamed.fold
            { err => println(s"[ERROR] ${err.userString}") }
            { expr =>
              import core.Printing.untyped.Ast
              import implied core.Printing.untyped.AstOps._
              pprintln(Convert[Tree, Ast](expr), height = Int.MaxValue)
              pprintln(rootCtx.toScoping, height = Int.MaxValue)
            }

          state
        }

      def guarded(string: String)(body: => LoopState): LoopState =
        if string.isEmpty then {
          println(s"[ERROR] empty input")
          state
        } else {
          body
        }

      parseCommand(input) match {
        case AstExpr(code) =>
          Ast(code)(parseExpr)
        case AstTop(code) =>
          Ast(code)(parseEEC)
        case TypeExpr(code) =>
          Typed(code)(parseExpr)
        case TypeTop(code) =>
          Typed(code)(parseEEC)
        case AstFile(name) => guarded(name) {
          import ContextOps._
          val rootCtx = new RootContext()
          implied for Context = rootCtx
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
              import core.Printing.untyped.Ast
              import implied core.Printing.untyped.AstOps._
              pprintln(Convert[Tree, Ast](ast), height = Int.MaxValue)
              pprintln(rootCtx.toScoping, height = Int.MaxValue)
            }

          state
        }
        case TypeFile(name) => Typed(name) { n =>
          for {
            code  <- loadFile(n)
            ast   <- parseEEC(code)
          } yield ast
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