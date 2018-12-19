package eec

object Main {

  import eec.repl.ArithmeticRepl

  def main(args: Array[String]): Unit = {
    if args.length == 1 && args(0) == "-help" then {
      println("Usage: eec [options]")
      println
      println("  -a    => run Arithmetic REPL")
      println("  -help => view these options")
    } else if args.contains("-a") then {
      new ArithmeticRepl().loop
    } else {
      println("No option specified. See options with -help")
    }
  }

}