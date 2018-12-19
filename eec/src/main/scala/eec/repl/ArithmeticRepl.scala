package eec
package repl

class ArithmeticRepl {
  import eec.compiler.ast.Arithmetic
  import eec.compiler.ast.Arithmetic._
  import eec.compiler.parseArithmetic
  import eec.interpreter.evalArithmetic
  import eec.compiler.exception._

  def loop: Unit = {
    println("starting eec REPL...")
    var break = false;
    while (!break) {
      print("eec> ")
      val input = readLine
      try {
        val parsed = parseArithmetic(input)
        val ans = evalArithmetic(parsed)
        println(s"eec> $ans")
      } catch {
        case UnexpectedEOF => Console.err.println(s"[ERROR] no valid expression input")
      }
    }
  }
}