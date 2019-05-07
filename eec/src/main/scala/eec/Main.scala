package eec

import repl._

object Main {

  def main(args: Array[String]): Unit = {
    if args `sameElements` Array("-help") then {
      println("Usage: eec <option>")
      println
      println("  -e    => run Mesa REPL")
      println("  -p    => modifier for -e to load the REPL with Prelude definitions.")
      println("  -help => view these options")
    } else if args `sameElements` Array("-e") then {
      Repl.loop(false)
    } else if args.toSet == Set("-e", "-p") then {
      Repl.loop(true)
    } else {
      println("No valid option specified. See options with -help")
    }
  }
}