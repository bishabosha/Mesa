package eec
package object interpreter {

  import eec.compiler.ast.Arithmetic
  import eec.compiler.ast.Arithmetic._

  def simplifyArithmetic(a: Arithmetic[_]): Arithmetic[Double] =
    Number(evalArithmetic(a))

  def evalArithmetic(a: Arithmetic[_]): Double = a match {
    case Number(n) => n
    case Expr(Op(op), l, r) => op match {
      case "*" => evalArithmetic(l) * evalArithmetic(r)
      case "+" => evalArithmetic(l) + evalArithmetic(r)
      case "-" => evalArithmetic(l) - evalArithmetic(r)
      case "/" => evalArithmetic(l) / evalArithmetic(r)
    }
  }

  def showArithmetic(a: Arithmetic[Double]): String = a match {
    case Number(n) => n.toString
    case Expr(Op(op), l, r) => op match {
      case "*" => s"(${showArithmetic(l)} * ${showArithmetic(r)})"
      case "+" => s"(${showArithmetic(l)} + ${showArithmetic(r)})"
      case "-" => s"(${showArithmetic(l)} - ${showArithmetic(r)})"
      case "/" => s"(${showArithmetic(l)} / ${showArithmetic(r)})"
    }
  }
}