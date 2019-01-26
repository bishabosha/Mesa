package eec
package object compiler {

  import org.antlr.v4.runtime._
  import ast._
  import EEC._

  def parseEEC(input: String): Ast = {
    val charStream = new ANTLRInputStream(input)
    val lexer = new EECLexer(charStream)
    val tokens = new CommonTokenStream(lexer)
    val parser = new EECParser(tokens)

    EECAstVisitor.visitTranslationUnit(parser.translationUnit())
  }
}