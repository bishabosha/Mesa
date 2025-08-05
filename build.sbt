lazy val commonSettings = Seq(
  scalaVersion := "3.7.2",
  libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test"
)

lazy val mesa = project
  .settings(
    name := "mesa-core",
    version := Common.mesaVersion,
    compileOrder := CompileOrder.JavaThenScala,
    libraryDependencies += ("com.lihaoyi" %% "pprint" % "0.9.3"),
    libraryDependencies += "org.scala-lang" % "scala-reflect" % "2.13.12",
    libraryDependencies ++= Seq(
      "org.antlr" % "antlr4-runtime" % "4.7.2",
      "org.antlr" % "stringtemplate" % "4.0.2"
    ),
    commonSettings
  )
  .dependsOn(util)

lazy val `eec-core` = project
  .settings(
    name := "mesa-eec",
    version := Common.mesaVersion,
    libraryDependencies += ("org.scala-lang.modules" %% "scala-parser-combinators" % "2.4.0"),
    commonSettings
  )
  .dependsOn(util)

lazy val util = project
  .settings(
    name := "mesa-util",
    version := Common.mesaVersion,
    commonSettings
  )

lazy val loadMesa = { s: State =>
  "project mesa" :: s
}

lazy val root = project
  .in(file("."))
  .settings(
    commonSettings,
    onLoad in Global := {
      val old = (onLoad in Global).value
      loadMesa compose old
    }
  )
