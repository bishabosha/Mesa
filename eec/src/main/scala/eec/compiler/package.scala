package eec
package object compiler {

  import org.antlr.v4.runtime._
  import ast.{Arithmetic, ArithmeticParser, ArithmeticVisitor, ArithmeticLexer}

  def parseArithmetic(input: String): Arithmetic[_] = {
    val charStream = new ANTLRInputStream(input)
    val lexer = new ArithmeticLexer(charStream)
    val tokens = new CommonTokenStream(lexer)
    val parser = new ArithmeticParser(tokens)

    ArithmeticVisitor.visit(parser.translationUnit())
  }
}