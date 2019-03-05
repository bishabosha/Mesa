package eec
package repl

class EECRepl {
  
  import Commands._
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
    import CompilerErrorOps._
    import implied CompilerErrorOps._

    def (prompt: String) asPrompt: String = s"$prompt> "

    @tailrec
    def inner(state: LoopState): Unit = state match {
      case s @ LoopState(prompt, false, _) =>
        println
        print(prompt.asPrompt)
        val nextState = command(s, readLine)
        inner(nextState)
      case LoopState(_, true, _) =>
        // exit
    }

    newContext.fold
      { err => println(s"[ERROR] ${err.userString}. Quitting...") }
      { ctx =>
        val initial = LoopState(defaultPrompt, false, ctx)
        println("starting eec REPL...")
        print(defaultPrompt.asPrompt)
        val state = command(initial, readLine)
        inner(state)
      }
  }

  private[this] def newContext: Checked[Context] = {
    import CompilerErrorOps._
    val ctx = new RootContext
    for (_ <- Context.enterBootstrapped given ctx) yield ctx
  }

  private[this] case class LoopState(prompt: String, break: Boolean, ctx: Context)

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
      import Command._
      import Typers._
      import ast.Trees._
      import core.Printing.untyped.AstOps._
      import CompilerErrorOps._
      import Context._
      import implied CompilerErrorOps._

      implied for Context = state.ctx

      def Typed(s: String)(f: String => Contextual[Checked[Tree]]): LoopState =
        guarded(s) {
          val yieldTyped = for {
            expr  <- f(s)
            _     <- indexAsExpr(expr).recoverDefault
            typed <- expr.typedAsExpr(Type.WildcardType)
          } yield typed

          yieldTyped.fold
            { error => println(s"[ERROR] ${error.userString}") }
            { typed =>
              import ContextOps._
              import implied TypeOps._
              println(typed.tpe.userString)
            }

          state
        }

      def Define(s: String): LoopState =
        guarded(s) {
          val typed = for {
            exp <- parseStat(s)
            _   <- indexAsExpr(exp).recoverDefault
            tpd <- exp.typedAsExpr(Type.WildcardType)
          } yield tpd

          typed.fold
            { err => println(s"[ERROR] ${err.userString}") }
            { tpd =>
              import Tree._
              import implied TypeOps._
              import implied core.Names.NameOps._
              val DefDef(_, DefSig(name, _), _, _) = tpd
              println(s"defined ${name.userString} : ${tpd.tpe.userString}")
            }

          state
        }

      def Ast(s: String)(f: String => Contextual[Checked[Tree]]): LoopState =
        guarded(s) {
          f(s).fold
            { err => println(s"[ERROR] ${err.userString}") }
            { ast =>
              import core.Printing.untyped.Ast
              import implied core.Printing.untyped.AstOps._
              pprintln(Convert[Tree, Ast](ast), height = Int.MaxValue)
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
          Ast(code)(parseStat)
        case AstFile(name) =>
          Ast(name) { n =>
            for {
              code  <- loadFile(n)
              ast   <- parseEEC(code)
            } yield ast
          }
        case Define(code) =>
          Define(code)
        case TypeExpr(code) =>
          Typed(code)(parseExpr)
        case TypeFile(name) =>
          Typed(name) { n =>
            for {
              code  <- loadFile(n)
              ast   <- parseEEC(code)
            } yield ast
          }
        case SetPrompt(newPrompt) =>
          guarded(newPrompt) { state.copy(prompt = newPrompt) }
        case Reset =>
          println("Loading a new context")
          newContext.fold
            { err =>
              println(s"[ERROR] ${err.userString}. Quitting...")
              state.copy(break = true) }
            { ctx => state.copy(ctx = ctx) }
        case Ctx =>
          import ContextOps._
          pprintln(ctx.toScoping, height = Int.MaxValue)
          state
        case Quit =>
          println("Quitting...")
          state.copy(break = true)
        case ShowHelp =>
          println(helpText)
          state
        case Unknown =>
          println(s"[ERROR] unrecognised command: `${trimOrEmpty(input)}`. Try `:help`")
          state
      }
    }
}