val dottyVersion = "0.17.0-RC1"
// val dottyVersion = dottyLatestNightlyBuild.get

lazy val commonSettings = Seq(
  scalaVersion := dottyVersion,

  libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test"
)

lazy val mesa = project
  .settings(
    name := "mesa-core",
    version := Common.mesaVersion,

    compileOrder := CompileOrder.JavaThenScala,

    libraryDependencies += ("com.lihaoyi" %% "pprint" % "0.5.3").withDottyCompat(scalaVersion.value),

    libraryDependencies += "org.scala-lang" % "scala-reflect" % "2.12.8",

    libraryDependencies ++= Seq(
      "org.antlr" % "antlr4-runtime" % "4.7.2",
      "org.antlr" % "stringtemplate" % "4.0.2"
    ),

    commonSettings,
  )

lazy val loadMesa = { s: State =>
  "project mesa" :: s
}

lazy val root = project.in(file("."))
  .settings(
    commonSettings,
    onLoad in Global := {
      val old = (onLoad in Global).value
      loadMesa compose old
    }
  )