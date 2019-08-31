package mesa

@main def Repl(args: String*) =
  if args.contains("-help") then {
    args.filterNot(_ == "-help").handleInvalid
    println("Usage: mesa <option>")
    println
    println("  -p    => the REPL will import Prelude definitions.")
    println("  -help => view these options")
  } else if args.contains("-p") then {
    args.filterNot(_ == "-p").handleInvalid
    repl.loop(true)
  } else if args.isEmpty then {
    repl.loop(false)
  } else {
    args.handleInvalid
    println("No valid option specified. See options with -help")
  }

private def (args: Seq[String]) handleInvalid: Unit =
  args.map("" + '"' + _ + '"').foreach(opt => println(s"Ignoring invalid option $opt."))