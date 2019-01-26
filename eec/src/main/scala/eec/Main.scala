package eec

object Main {
  def main(args: Array[String]): Unit = {
    import eec.repl.EECRepl
    if args.length == 1 && args(0) == "-help" then {
      println("Usage: eec [options]")
      println
      println("  -e    => run EEC REPL")
      println("  -help => view these options")
    } else if args.contains("-e") && !args.contains("-a") then {
      new EECRepl().loop
    } else {
      println("No option specified. See options with -help")
    }
  }
}