enablePlugins(Antlr4Plugin)

antlr4PackageName in Antlr4 := Some("mesa.compiler.parsing")
antlr4GenListener in Antlr4 := false // default: true
antlr4GenVisitor in Antlr4  := false // default: false