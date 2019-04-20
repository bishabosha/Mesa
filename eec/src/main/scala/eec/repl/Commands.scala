package eec
package repl


object Commands {
  import Command._

  enum Command derives Eql {
    case AstExpr(code: String)
    case AstTop(code: String)
    case TypeExpr(code: String)
    case Define(code: String)
    case AstFile(path: String)
    case TypeFile(path: String)
    case SetPrompt(prompt: String)
    case Reset, Quit, ShowHelp, Ctx, Unknown
  }

  val helpText: String =
    """The REPL has several commands available:
      |
      | :help           => Show this help
      | :ast    <expr>  => Print the AST for the given expression
      | :astt   <topl>  => Print the AST for the given top level declarations
      | :astf   <file>  => Print the AST for the given code loaded from file
      | :t      <expr>  => Print the Type for the given expression
      | :tf     <file>  => Print the Type for the given code loaded from file
      | :def    <topl>  => Define a new top level statement in the _root_ package
      | :prompt <word>  => Change the REPL prompt
      | :reset          => Reset the context of the REPL session
      | :ctx            => Show the current Context
      | :q              => Quit the REPL""".stripMargin

  def parseCommand(line: String): Command = {
    import Parsers._

    def (s: String) trimOrEmpty: String = Option(s).fold("")(_.trim)

    object Parsers {
      val resetCommand  = """:reset(?:\s*)""".r
      val quitCommand   = """:q(?:\s*)""".r
      val astExpr       = """:ast(?:\s+?(.*))?""".r
      val astTop        = """:astt(?:\s+?(.*))?""".r
      val typeExpr      = """:t(?:\s+?(.*))?""".r
      val define        = """:def(?:\s+?(.*))?""".r
      val astFile       = """:astf(?:\s+((?:\S(?:\s*)?)*))?""".r
      val typeFile      = """:tf(?:\s+((?:\S(?:\s*)?)*))?""".r
      val setPrompt     = """:prompt(?:\s*?(\S*))?""".r
      val ctx           = """:ctx(?:\s*)""".r
      val showHelp      = """:help(?:\s*)""".r
    }

    line.trim match {
      case quitCommand()        => Quit
      case astExpr(code)        => AstExpr(code.trimOrEmpty)
      case astTop(code)         => AstTop(code.trimOrEmpty)
      case typeExpr(code)       => TypeExpr(code.trimOrEmpty)
      case define(code)         => Define(code.trimOrEmpty)
      case astFile(file)        => AstFile(file.trimOrEmpty)
      case typeFile(file)       => TypeFile(file.trimOrEmpty)
      case setPrompt(newPrompt) => SetPrompt(newPrompt.trimOrEmpty)
      case ctx()                => Ctx
      case showHelp()           => ShowHelp
      case resetCommand()       => Reset
      case _                    => Unknown
    }
  }
}
