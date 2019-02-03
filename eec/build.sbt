val dottyVersion = "0.12.0-RC1"

enablePlugins(Antlr4Plugin)

antlr4PackageName in Antlr4 := Some("eec.compiler.ast")
antlr4GenListener in Antlr4 := false // default: true
antlr4GenVisitor in Antlr4 := true // default: false



lazy val root = project
  .in(file("."))
  .settings(
    name := "eec",
    version := "0.1.0",

    compileOrder := CompileOrder.JavaThenScala,

    scalaVersion := dottyVersion,

    libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test",

    libraryDependencies += ("com.lihaoyi" %% "pprint" % "0.5.3").withDottyCompat(scalaVersion.value),

    libraryDependencies += "org.scala-lang" % "scala-reflect" % "2.12.8",

    libraryDependencies ++= Seq(
      "org.antlr" % "antlr4-runtime" % "4.7.1",
      "org.antlr" % "stringtemplate" % "4.0.2"
    ),

    // libraryDependencies += ("org.scalaz" %% "scalaz-zio" % "0.5.1").withDottyCompat(scalaVersion.value)

  )
