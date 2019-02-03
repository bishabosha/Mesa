package eec
package repl

object Commands {
  import Command._

  enum Command {
    case PrintAst(code: String)
    case PrintFile(path: String)
    case SetPrompt(prompt: String)
    case Quit
    case ShowHelp
    case Unknown
  }

  val helpText =
    """REPL: Read Eval Print Loop
      |Commands
      |========
      | :help           => Show this help
      | :p      <code>  => Print the AST for the given code
      | :loadp  <file>  => Print the AST for the given code loaded from file
      | :prompt /(\S*)/ => Change the REPL prompt
      | :q              => Quit the REPL""".stripMargin

  private val quitCommand = ":q".r
  private val printAst = """:p\s+?(.*)""".r
  private val printFile = """:loadp\s+((?:\S(?:\s*)?)*)""".r
  private val setPrompt = """:prompt\s*?(\S*)""".r
  private val showHelp = ":help".r

  def parseCommand(line: String): Command = line match {
    case quitCommand() => Quit
    case printAst(code) => PrintAst(code)
    case printFile(file) => PrintFile(file)
    case setPrompt(newPrompt) => SetPrompt(newPrompt)
    case showHelp() => ShowHelp
    case _ => Unknown
  }
}