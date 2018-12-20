package eec

object Main {

  import eec.repl.ArithmeticRepl
  import eec.repl.EECRepl

  def main(args: Array[String]): Unit = {
    if args.length == 1 && args(0) == "-help" then {
      println("Usage: eec [options]")
      println
      println("  -a    => run Arithmetic REPL")
      println("  -e    => run EEC REPL")
      println("  -help => view these options")
    } else if args.contains("-a") && !args.contains("-e") then {
      new ArithmeticRepl().loop
    } else if args.contains("-e") && !args.contains("-a") then {
      new EECRepl().loop
    } else {
      println("No option specified. See options with -help")
    }
  }

}