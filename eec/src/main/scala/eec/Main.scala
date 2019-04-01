package eec

import repl._

object Main {

  def main(args: Array[String]): Unit = {
    if args `sameElements` Array("-help") then {
      println("Usage: eec <option>")
      println
      println("  -e    => run EEC REPL")
      println("  -help => view these options")
    } else if args `sameElements` Array("-e") then {
      Repl.loop()
    } else {
      println("No valid option specified. See options with -help")
    }
  }
}