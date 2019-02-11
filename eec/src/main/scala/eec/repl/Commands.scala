package eec
package repl

object Commands {
  import Command._

  enum Command {
    case AstExpr(code: String)
    case TypeExpr(code: String)
    case AstFile(path: String)
    case SetPrompt(prompt: String)
    case Quit
    case ShowHelp
    case Unknown
  }

  val helpText =
    """The REPL has several commands available:
      |
      | :help           => Show this help
      | :ast    <expr>  => Print the AST for the given expression
      | :t      <expr>  => Print the Type for the given expression
      | :astf   <file>  => Print the AST for the given code loaded from file
      | :prompt <word>  => Change the REPL prompt
      | :q              => Quit the REPL""".stripMargin

  private val quitCommand = ":q".r
  private val astExpr = """:ast(?:\s+?(.*))?""".r
  private val typeExpr = """:t(?:\s+?(.*))?""".r
  private val astFile = """:astf(?:\s+((?:\S(?:\s*)?)*))?""".r
  private val setPrompt = """:prompt(?:\s*?(\S*))?""".r
  private val showHelp = ":help".r

  def (s: String) trimOrEmpty: String = Option(s).map(_.trim).getOrElse("")

  def parseCommand(line: String): Command = line.trim match {
    case quitCommand() => Quit
    case astExpr(code) => AstExpr(code.trimOrEmpty)
    case typeExpr(code) => TypeExpr(code.trimOrEmpty)
    case astFile(file) => AstFile(file.trimOrEmpty)
    case setPrompt(newPrompt) => SetPrompt(newPrompt.trimOrEmpty)
    case showHelp() => ShowHelp
    case _ => Unknown
  }
}