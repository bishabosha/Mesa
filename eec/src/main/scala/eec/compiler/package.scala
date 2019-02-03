package eec
package object compiler {

  import org.antlr.v4.runtime._
  import ast._
  import Trees._
  import eec.compiler.exception._

  object EecErrorListener extends BaseErrorListener {
    override def syntaxError(
      recognizer: Recognizer[_, _],
      offendingSymbol: AnyRef,
      line: Int,
      charPositionInLine: Int,
      msg: String,
      e: RecognitionException): Unit = {
        import eec.compiler.exception._
        throw new SyntaxError(
          "line " + line + ":" + charPositionInLine + " " + msg)
      }
  }

  private def parser[O](f: (EECTreeVisitor, EECParser) => O)(input: String): O = {
    val charStream = new ANTLRInputStream(input)
    val lexer = new EECLexer(charStream)
    val tokens = new CommonTokenStream(lexer)
    val parser = new EECParser(tokens)
    parser.removeErrorListeners
    parser.addErrorListener(EecErrorListener)
    f(new EECTreeVisitor(), parser)
  }

  def parseEEC(input: String): Tree | EECError = {
    val eecParser = parser((v, p) => (
      v.visitTranslationUnit(p.translationUnit())
    ))
    recover { eecParser(input) }
  }

  def parseExpr(input: String): ExprTree | EECError = {
    val exprParser = parser((v, p) => v.visitExpr(p.expr()))
    recover { exprParser(input) }
  }

  def typ(expr: ExprTree): String | EECError = {
    new TypeError("Types are not implemented")
  }

  val preludeSource ="""
package eec.Prelude

primitive fst p: (a, b) -> a
primitive snd p: (a, b) -> b
primitive debug x: a -> String

Unit: () = ()
""".trim
}