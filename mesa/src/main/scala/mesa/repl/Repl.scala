package mesa.repl

import scala.language.implicitConversions

import scala.util.control.NonFatal
import scala.annotation.tailrec
import scala.io.StdIn.readLine

import Commands.{Command, parseCommand, helpText}
import Command._
import mesa.compiler.{ast, core, parsing, error, types}
import ast.Trees.{Tree, TreeOps}
import Tree._
import core.{Meta, Contexts, Names}
import Meta.TreeOps._
import Meta.ContextOps._
import Contexts.{Context, IdGen, RootContext, IdReader}
import Names.{Name, NameOps}
import Context._
import parsing.{parseMesa, parseDef, parseExpr}
import error.CompilerErrors.{CompilerError, CompilerErrorOps, Lifted}
import CompilerErrorOps._
import types.{Namers, Typers, Types, Prelude}
import Namers.indexed
import Typers.typed
import Types.{Type, TypeOps}
import Type._
import java.nio.file.{Paths => JPaths}
import java.nio.file.{Files => JFiles}
import java.io.{File => JFile}
import java.util.stream.Collectors

import CompilerErrorOps.given
import TreeOps.given
import TypeOps.given
import NameOps.given
import Meta.TreeOps.given
import Meta.ContextOps.given

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

private val defaultPrompt = "mesa"

extension (msg: String) private def wrapErr =
  Console.RED + "[ERROR] " + msg + Console.RESET

def loop(enterPrelude: Boolean): Unit = {

  extension (prompt: String) def asPrompt: String = s"$prompt${Console.CYAN}>${Console.RESET} "

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

  println("starting mesa REPL...")
  newContext(enterPrelude).fold
    { err => println(s"${err.show}. Quitting...".wrapErr) }
    { (idGen, ctx) =>
      val pwd = JPaths.get("").toAbsolutePath.toString
      val initial = LoopState(defaultPrompt, false, pwd, enterPrelude, idGen, ctx)
      print(defaultPrompt.asPrompt)
      val state = command(initial, readLine)
      inner(state)
    }
}

private def newContext(enterPrelude: Boolean): Lifted[(IdGen, Context)] = {
  val rootCtx   = new RootContext()
  val rootIdGen = new IdGen
  given Context = rootCtx
  given IdGen = rootIdGen
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

  given Context = state.ctx
  given IdGen   = state.idGen

  def Typed(s: String)(f: String => IdReader[Lifted[Tree]]): LoopState = {
    guarded(state, s) {
      val yieldTyped =
        for
          expr  <- f(s)
          _     <- expr.indexed
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
      val typed =
        for
          exp <- parseDef(s)
          _   <- exp.indexed
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
          ast   <- parseMesa(code)
        yield ast
      }

    case Define(code)   => Define(code)
    case TypeExpr(code) => Typed(code)(parseExpr)

    case TypeFile(name) =>
      Typed(name) { n =>
        for
          code  <- loadFile(state.pwd, n)
          ast   <- parseMesa(code)
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

    case Pwd =>
      println(state.pwd)
      state

    case ChangeDirectory(dir) =>
      changeDirectory(state.pwd, dir).fold
        { err =>
          println(err.show.wrapErr)
          state
        }
        { pwd => println(s"Changed directory to $pwd")
          state.copy(pwd = pwd)
        }

    case Quit =>
      println("Quitting...")
      state.copy(break = true)

    case ShowHelp =>
      println(helpText)
      state

    case Unknown(input) =>
      if input.isEmpty then {
        state
      } else {
        println(s"unrecognised command: `$input`. Try `:help`.".wrapErr)
        state
      }
  }
}

private val relative = s"(?!${JFile.separator}).*".r

private def changeDirectory(pwd: String, path: String): Lifted[String] = liftedIO {
  path match {
    case ""                                                      => JPaths.get("").toAbsolutePath.toString
    case relative() if JFiles.isDirectory(JPaths.get(pwd, path)) => JPaths.get(pwd, path).normalize.toString
    case _          if JFiles.isDirectory(JPaths.get(path))      => JPaths.get(path).normalize.toString

    case _ => CompilerError.IllegalInput(s"Not a directory: $path")
  }
}

private def loadFile(pwd: String, path: String): Lifted[String] = liftedIO {
  path match {
    case relative() if JFiles.isReadable(JPaths.get(pwd, path)) =>
      JFiles.lines(JPaths.get(pwd, path)).collect(Collectors.joining("\n"))

    case _ if JFiles.isReadable(JPaths.get(path)) =>
      JFiles.lines(JPaths.get(path)).collect(Collectors.joining("\n"))

    case _ => CompilerError.IllegalInput(s"Not a file: $path")
  }
}

private inline def liftedIO[T](op: => Lifted[T]): Lifted[T] = {
  try op
  catch {
    case e: java.io.IOException => CompilerError.IllegalInput(e.getMessage)
    case NonFatal(e)            => CompilerError.Internal(e.getMessage)
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
