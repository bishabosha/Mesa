package eec
package object compiler {

  import org.antlr.v4.runtime._
  import ast._
  import Trees._

  def parseEEC(input: String): Tree = {
    val charStream = new ANTLRInputStream(input)
    val lexer = new EECLexer(charStream)
    val tokens = new CommonTokenStream(lexer)
    val parser = new EECParser(tokens)

    new EECTreeVisitor().visitTranslationUnit(parser.translationUnit())
  }
}