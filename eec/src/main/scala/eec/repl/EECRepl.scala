package eec
package repl

class EECRepl {
  import eec.compiler.ast.EEC
  import eec.compiler.ast.EEC._
  import eec.compiler.parseEEC
  import eec.compiler.exception._

  def loop: Unit = {
    println("starting eec REPL...")
    var break = false;
    while (!break) {
      print("eec> ")
      val input = readLine
      try {
        val parsed = parseEEC(input)
        println(s"eec> $parsed")
      } catch {
        case UnexpectedEOF => Console.err.println(s"[ERROR] no valid expression input")
        case UnexpectedInput(msg) => Console.err.println(s"[ERROR] unexpected input: $msg")
      }
    }
  }
}