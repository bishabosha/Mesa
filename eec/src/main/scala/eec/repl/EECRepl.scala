package eec
package repl

class EECRepl {
  import eec.compiler._
  import eec.compiler.exception._
  import pprint._

  def loop: Unit = {
    println("starting eec REPL...")
    var break = false;
    var prompt = "eec"
    val pwd = System.getProperty("user.dir")
    def command(input: String): Unit = {
      import Commands._
      import Commands.Command._
      import eec.compiler.ast.Trees._

      parseCommand(input) match {
        case AstExpr(code) =>
          parseExpr(code) match {
            case e: EECError =>
              println(s"[ERROR] ${e.getMessage}")
            case expr =>
              pprintln(expr, height = Int.MaxValue)
          }
        case TypeExpr(code) =>
          parseExpr(code) match {
            case e: EECError =>
              println(s"[ERROR] ${e.getMessage}")
            case expr: ExprTree =>
              typ(expr) match {
                case e: EECError =>
                  println(s"[ERROR] ${e.getMessage}")
                case e: String =>
                  println(e)
              }
          }
        case AstFile(name) =>
          val file = scala.io.Source.fromFile(s"$pwd/$name")
          val code = file.getLines.mkString("\n")
          file.close()
          parseEEC(code) match {
            case e: EECError =>
              println(s"[ERROR] ${e.getMessage}")
            case ast =>
              pprintln(ast, height = Int.MaxValue)
          }
        case SetPrompt(newPrompt) =>
          prompt = newPrompt
        case Quit =>
          println("Quitting...")
          break = true
        case ShowHelp =>
          println(helpText)
        case Unknown =>
          println(s"[ERROR] unrecognised command: `$input`. Try `:help`")
      }
    }
    while (!break) {
      print(s"$prompt> ")
      command(readLine)
    }
  }
}