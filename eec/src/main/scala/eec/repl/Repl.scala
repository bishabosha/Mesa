package eec
package repl
  
import Commands._
import compiler._
import types._
import parsing.EntryPoint._
import Namers._
import Types._
import Type._
import core.Contexts._
import error.CompilerErrors._
import scala.annotation._
import util.Convert
import pprint._

object Repl {

  private[this] val defaultPrompt = "eec"

  def loop(): Unit = {
    import CompilerErrorOps._
    import implied CompilerErrorOps._

    def (prompt: String) asPrompt: String = s"$prompt> "

    @tailrec
    def inner(state: LoopState): Unit = state match {
      case s @ LoopState(prompt, false, _, _, _) =>
        println
        print(prompt.asPrompt)
        val nextState = command(s, readLine)
        inner(nextState)
      case LoopState(_, true, _, _, _) =>
        // exit
    }

    newContext.fold
      { err => println(s"[ERROR] ${err.userString}. Quitting...") }
      { (idGen, ctx) =>
        val pwd = System.getProperty("user.dir")
        val initial = LoopState(defaultPrompt, false, pwd, idGen, ctx)
        println("starting eec REPL...")
        print(defaultPrompt.asPrompt)
        val state = command(initial, readLine)
        inner(state)
      }
  }

  private[this] def newContext: Checked[(IdGen, Context)] = {
    import CompilerErrorOps._
    val rootCtx = new RootContext
    val rootIdGen = new IdGen
    implied for Context = rootCtx
    implied for IdGen = rootIdGen
    for (_ <- Context.enterBootstrapped) yield (rootIdGen, rootCtx)
  }

  private[this] case class LoopState(prompt: String, break: Boolean, pwd: String, idGen: IdGen, ctx: Context)

  private[this] def loadFile(pwd: String, name: String): Checked[String] = {
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

  private[this] def guarded(state: LoopState, string: String)(body: => LoopState): LoopState =
    if string.isEmpty then {
      println(s"[ERROR] empty input")
      state
    } else {
      body
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
    implied for IdGen   = state.idGen

    def Typed(s: String)(f: String => IdMaker[Checked[Tree]]): LoopState =
      guarded(state, s) {
        val yieldTyped = for {
          expr  <- f(s)
          _     <- indexAsExpr(expr)
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
      guarded(state, s) {
        val typed = for {
          exp <- parseStat(s)
          _   <- indexAsExpr(exp)
          tpd <- exp.typedAsExpr(WildcardType)
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

    def Ast(s: String)(f: String => IdMaker[Checked[Tree]]): LoopState =
      guarded(state, s) {
        f(s).fold
          { err => println(s"[ERROR] ${err.userString}") }
          { ast =>
            import core.Printing.untyped.Ast
            import implied core.Printing.untyped.AstOps._
            pprintln(Convert[Tree, Ast](ast), height = Int.MaxValue)
          }

        state
      }

    parseCommand(input) match {
      case AstExpr(code) =>
        Ast(code)(parseExpr)
      case AstTop(code) =>
        Ast(code)(parseStat)
      case AstFile(name) =>
        Ast(name) { n =>
          for {
            code  <- loadFile(state.pwd, n)
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
            code  <- loadFile(state.pwd, n)
            ast   <- parseEEC(code)
          } yield ast
        }
      case SetPrompt(newPrompt) =>
        guarded(state, newPrompt) { state.copy(prompt = newPrompt) }
      case Reset =>
        println("Loading a new context")
        newContext.fold
          { err =>
            println(s"[ERROR] ${err.userString}. Quitting...")
            state.copy(break = true) }
          { (idGen, ctx) => state.copy(idGen = idGen, ctx = ctx) }
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