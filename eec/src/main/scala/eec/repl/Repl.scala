package eec.repl

import scala.language.implicitConversions

import scala.util.control.NonFatal
import scala.annotation.tailrec
  
import Commands.{Command, parseCommand, helpText}
import Command._
import eec.compiler.{ast, core, parsing, error, types}
import ast.Trees.{Tree, TreeOps}
import Tree._
import core.{Meta, Contexts, Names}
import Meta.TreeOps._
import Meta.ContextOps._
import Contexts.{Context, IdGen, RootContext, IdReader}
import Names.{Name, NameOps}
import Context._
import parsing.EntryPoint.{parseEEC, parseDef, parseExpr}
import error.CompilerErrors.{CompilerError, CompilerErrorOps, Lifted}
import CompilerErrorOps._
import types.{Namers, Typers, Types, Prelude}
import Namers.indexed
import Typers.typed
import Types.{Type, TypeOps}
import Type._

import implied CompilerErrorOps._
import implied TreeOps._
import implied TypeOps._
import implied NameOps._
import implied Meta.TreeOps._
import implied Meta.ContextOps._

object Repl {
  import pprint2.pprintln

  private val pprint2: pprint.PPrinter = pprint.copy(
    defaultHeight = Int.MaxValue,
    defaultWidth  = 80,
    additionalHandlers = {
      case s: String => pprint.Tree.Literal(s)
    }
  )

  private case class LoopState(
    prompt: String,
    break: Boolean,
    pwd: String,
    loadPrelude: Boolean,
    idGen: IdGen,
    ctx: Context
  )

  private val defaultPrompt = "eec"

  private def (msg: String) wrapErr =
    Console.RED + "[ERROR] " + msg + Console.RESET

  def loop(enterPrelude: Boolean): Unit = {

    def (prompt: String) asPrompt: String = s"$prompt> "

    @tailrec
    def inner(state: LoopState): Unit = state match {
      case s @ LoopState(prompt,false,_,_,_,_) =>
        println
        print(prompt.asPrompt)
        val nextState = command(s, readLine)
        inner(nextState)

      case LoopState(_,true,_,_,_,_) =>
        // exit
    }

    println("starting eec REPL...")
    newContext(enterPrelude).fold
      { err => println(s"${err.show}. Quitting...".wrapErr) }
      { (idGen, ctx) =>
        val pwd = System.getProperty("user.dir")
        val initial = LoopState(defaultPrompt, false, pwd, enterPrelude, idGen, ctx)
        print(defaultPrompt.asPrompt)
        val state = command(initial, readLine)
        inner(state)
      }
  }

  private def newContext(enterPrelude: Boolean): Lifted[(IdGen, Context)] = {
    val rootCtx   = new RootContext()
    val rootIdGen = new IdGen
    implied for Context = rootCtx
    implied for IdGen = rootIdGen
    for
      _ <- Context.enterBootstrapped
      _ <- lift {
        if enterPrelude then {
          println("Loading Prelude.")
          Prelude.importInScope
        } else {
          ()
        }
      }
    yield (rootIdGen, rootCtx)
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
          { error => println(error.show.wrapErr) }
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
          { err => println(err.show.wrapErr) }
          { tpd =>
            val name: Name = tpd
            println(s"defined ${name.define} : ${tpd.tpe.show}")
          }

        state
      }
    }

    def Ast(s: String)(f: String => IdReader[Lifted[Tree]]): LoopState = {
      guarded(state, s) {
        f(s).fold
          { err => println(err.show.wrapErr) }
          { ast => pprintln((ast: Meta.Tree)) }

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
        println("Loading a new context.")
        newContext(state.loadPrelude).fold
          { err =>
            println(s"${err.show}. Quitting...".wrapErr)
            state.copy(break = true) }
          { (idGen, ctx) => state.copy(idGen = idGen, ctx = ctx) }

      case Ctx =>
        pprintln(ctx: Seq[Meta.Context])
        state

      case Quit =>
        println("Quitting...")
        state.copy(break = true)

      case ShowHelp =>
        println(helpText)
        state

      case Unknown(input) =>
        guarded(state, input) {
          println(s"unrecognised command: `$input`. Try `:help`.".wrapErr)
          state
        }
    }
  }

  private def loadFile(pwd: String, name: String): Lifted[String] = {
    var file: scala.io.BufferedSource = null
    try {
      file = scala.io.Source.fromFile(s"$pwd/$name")
      return file.getLines.mkString("\n")
    } catch {
      case e: Exception if NonFatal(e) =>
        CompilerError.IllegalInput(e.getMessage)
    } finally {
      if file ne null then {
        file.close()
      }
    }
  }

  private def guarded(state: LoopState, string: String)
                     (body: => LoopState): LoopState = {
    if string.isEmpty then {
      println("empty input.".wrapErr)
      state
    } else {
      body
    }
  }
}