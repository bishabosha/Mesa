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
    while (!break) {
      print(s"$prompt> ")
      val input = readLine
      try {
        import Commands._
        import Commands.Command._
        parseCommand(input) match {
          case PrintAst(code) =>
            val parsed = parseEEC(code)
            pprintln(parsed, height = Int.MaxValue)
          case PrintFile(name) =>
            val file = scala.io.Source.fromFile(s"$pwd/$name")
            val code = file.getLines.mkString("\n")
            file.close()
            val parsed = parseEEC(code)
            pprintln(parsed, height = Int.MaxValue)
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
      } catch {
        case UnexpectedEOF =>
          Console.err.println(s"[ERROR] no valid expression input")
        case UnexpectedInput(msg) =>
          Console.err.println(s"[ERROR] unexpected input: $msg")
      }
    }
  }
}