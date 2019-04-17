package eec
package repl

import scala.util.control.NonFatal
import scala.annotation.tailrec
  
import Commands._
import Command._
import compiler._
import ast.Trees._
import Tree._
import core.Printing.untyped._
import AstOps._
import core.Contexts._
import core.Names._
import Context._
import ContextOps._
import parsing.EntryPoint.{parseEEC, parseStat, parseExpr}
import error.CompilerErrors._
import CompilerErrorOps._
import types.{Namers, Typers, Types}
import Namers._
import Typers._
import Types._
import Type._
import util.Convert
import Convert._

import implied CompilerErrorOps._
import implied TypeOps._
import implied NameOps._
import implied AstOps._
import implied TreeOps._

object Repl {
  import pprint2.pprintln

  private val pprint2: pprint.PPrinter = pprint.copy(
    defaultHeight = Int.MaxValue,
    additionalHandlers = {
      case s: String => pprint.Tree.Literal(s)
    }
  )

  private case class LoopState(
    prompt: String,
    break: Boolean,
    pwd: String,
    idGen: IdGen,
    ctx: Context
  )

  private val defaultPrompt = "eec"

  def loop(): Unit = {

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
      { err => println(s"[ERROR] ${err.show}. Quitting...") }
      { (idGen, ctx) =>
        val pwd = System.getProperty("user.dir")
        val initial = LoopState(defaultPrompt, false, pwd, idGen, ctx)
        println("starting eec REPL...")
        print(defaultPrompt.asPrompt)
        val state = command(initial, readLine)
        inner(state)
      }
  }

  private def newContext: Checked[(IdGen, Context)] = {
    val rootCtx   = new RootContext()
    val rootIdGen = new IdGen
    implied for Context = rootCtx
    implied for IdGen = rootIdGen
    for _ <- Context.enterBootstrapped yield (rootIdGen, rootCtx)
  }

  private def command(state: LoopState, input: String): LoopState = {

    implied for Context = state.ctx
    implied for IdGen   = state.idGen

    def Typed(s: String)(f: String => IdReader[Checked[Tree]]): LoopState = {
      guarded(state, s) {
        val yieldTyped = for
          expr  <- f(s)
          _     <- indexAsExpr(expr)
          typed <- expr.typedWith(WildcardType)
        yield typed

        yieldTyped.fold
          { error => println(s"[ERROR] ${error.show}") }
          { typed => println(typed.tpe.show) }

        state
      }
    }

    def Define(s: String): LoopState = {
      guarded(state, s) {
        val typed = for
          exp <- parseStat(s)
          _   <- indexAsExpr(exp)
          tpd <- exp.typedWith(WildcardType)
        yield tpd

        typed.fold
          { err => println(s"[ERROR] ${err.show}") }
          { tpd =>
            val DefDef(_, sig, _, _) = tpd
            val name: Name = sig.convert
            println(s"defined ${name.show} : ${tpd.tpe.show}")
          }

        state
      }
    }

    def Ast(s: String)(f: String => IdReader[Checked[Tree]]): LoopState = {
      guarded(state, s) {
        f(s).fold
          { err => println(s"[ERROR] ${err.show}") }
          { ast => pprintln((ast.convert: Ast)) }

        state
      }
    }

    parseCommand(input) match {
      case AstExpr(code) => Ast(code)(parseExpr)
      case AstTop(code)  => Ast(code)(parseStat)

      case AstFile(name) =>
        Ast(name) { n =>
          for
            code  <- loadFile(state.pwd, n)
            ast   <- parseEEC(code)
          yield ast
        }

      case Define(code)   => Define(code)
      case TypeExpr(code) => Typed(code)(parseExpr)

      case TypeFile(name) =>
        Typed(name) { n =>
          for
            code  <- loadFile(state.pwd, n)
            ast   <- parseEEC(code)
          yield ast
        }

      case SetPrompt(newPrompt) =>
        guarded(state, newPrompt) { state.copy(prompt = newPrompt) }

      case Reset =>
        println("Loading a new context")
        newContext.fold
          { err =>
            println(s"[ERROR] ${err.show}. Quitting...")
            state.copy(break = true) }
          { (idGen, ctx) => state.copy(idGen = idGen, ctx = ctx) }

      case Ctx =>
        pprintln(ctx.toScoping)
        state

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

  private def loadFile(pwd: String, name: String): Checked[String] = {
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

  private def guarded(state: LoopState, string: String)
                     (body: => LoopState): LoopState = {
    if string.isEmpty then {
      println(s"[ERROR] empty input")
      state
    } else {
      body
    }
  }
}