package eec
package repl

class EECRepl {
  
  import compiler.types._
  import compiler.parsers._
  import compiler.errors.ParserErrors.ParserError
  import compiler.errors.TyperErrors.TyperError
  import compiler.types.Types._
  import compiler.types.Types.Type
  import scala.annotation._
  import pprint._

  val pwd = System.getProperty("user.dir")
  val defaultPrompt = "eec"

  def loop: Unit = {
    @tailrec
    def inner(state: LoopState): Unit = state match {
      case s @ LoopState(prompt, false) =>
        print(s"$prompt> ")
        inner(command(s, readLine))
      case LoopState(_, true) =>
        // exit
    }
    println("starting eec REPL...")
    inner(LoopState(defaultPrompt, false))
  }

  private[this] case class LoopState(prompt: String, break: Boolean)

  private def command(state: LoopState, input: String): LoopState = {
      import Commands._
      import Commands.Command._
      import compiler.ast.Trees._

      parseCommand(input) match {
        case AstExpr(code) =>
          parseExpr(code) match {
            case e: ParserError =>
              println(s"[ERROR] ${e.userString}")
              state
            case expr =>
              pprintln(expr, height = Int.MaxValue)
              state
          }
        case TypeExpr(code) =>
          parseExpr(code) match {
            case e: ParserError =>
              println(s"[ERROR] ${e.userString}")
              state
            case expr: Tree =>
              expr.typ match {
                case e: TyperError =>
                  println(s"[ERROR] ${e.userString}")
                  state
                case t: Type =>
                  println(t.userString)
                  state
              }
          }
        case AstFile(name) =>
          val file = scala.io.Source.fromFile(s"$pwd/$name")
          val code = file.getLines.mkString("\n")
          file.close()
          parseEEC(code) match {
            case e: ParserError =>
              println(s"[ERROR] ${e.userString}")
              state
            case ast =>
              pprintln(ast, height = Int.MaxValue)
              state
          }
        case SetPrompt(newPrompt) =>
          state.copy(prompt = newPrompt)
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