package eec
package repl

import scala.language.implicitConversions

import scala.util.control.NonFatal
import scala.annotation.tailrec
  
import Commands._
import Command._
import compiler._
import ast.Trees._
import Tree._
import core.{Stable, Contexts, Names}
import Stable.TreeOps._
import Stable.ContextOps._
import Contexts._
import Names._
import Context._
import parsing.EntryPoint.{parseEEC, parseDef, parseExpr}
import error.CompilerErrors._
import CompilerErrorOps._
import types.{Namers, Typers, Types}
import Namers._
import Typers._
import Types._
import Type._

import implied CompilerErrorOps._
import implied TreeOps._
import implied TypeOps._
import implied NameOps._
import implied Stable.TreeOps._
import implied Stable.ContextOps._

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

  private def newContext: Lifted[(IdGen, Context)] = {
    val rootCtx   = new RootContext()
    val rootIdGen = new IdGen
    implied for Context = rootCtx
    implied for IdGen = rootIdGen
    for _ <- Context.enterBootstrapped yield (rootIdGen, rootCtx)
  }

  private def command(state: LoopState, input: String): LoopState = {

    implied for Context = state.ctx
    implied for IdGen   = state.idGen

    def Typed(s: String)(f: String => IdReader[Lifted[Tree]]): LoopState = {
      guarded(state, s) {
        val yieldTyped = for
          expr  <- f(s)
          _     <- indexed(expr)
          typed <- expr.typed
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
          exp <- parseDef(s)
          _   <- indexed(exp)
          tpd <- exp.typed
        yield tpd

        typed.fold
          { err => println(s"[ERROR] ${err.show}") }
          { tpd =>
            val DefDef(_, sig, _, _) = tpd
            val name: Name = sig
            println(s"defined ${name.show} : ${tpd.tpe.show}")
          }

        state
      }
    }

    def Ast(s: String)(f: String => IdReader[Lifted[Tree]]): LoopState = {
      guarded(state, s) {
        f(s).fold
          { err => println(s"[ERROR] ${err.show}") }
          { ast => pprintln((ast: Stable.Tree)) }

        state
      }
    }

    parseCommand(input) match {
      case AstExpr(code) => Ast(code)(parseExpr)
      case AstTop(code)  => Ast(code)(parseDef)

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
        pprintln(ctx: Seq[Stable.Context])
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

  private def loadFile(pwd: String, name: String): Lifted[String] = {
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
      println("[ERROR] empty input")
      state
    } else {
      body
    }
  }
}