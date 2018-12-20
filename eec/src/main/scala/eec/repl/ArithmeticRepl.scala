package eec
package repl

class ArithmeticRepl {
  import eec.compiler.ast.Arithmetic
  import eec.compiler.ast.Arithmetic._
  import eec.compiler.parseArithmetic
  import eec.interpreter.evalArithmetic
  import eec.compiler.exception._

  def loop: Unit = {
    println("starting arithmetic REPL...")
    var break = false;
    while (!break) {
      print("arithmetic> ")
      val input = readLine
      try {
        val parsed = parseArithmetic(input)
        val ans = evalArithmetic(parsed)
        println(s"arithmetic> $ans")
      } catch {
        case UnexpectedEOF => Console.err.println(s"[ERROR] no valid expression input")
      }
    }
  }
}