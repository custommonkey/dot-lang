lazy val root = (project in file("."))
  .settings(
    name := "dot-lang",
    scalaVersion := "2.12.1",
    coverageEnabled := true
  )

libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.1"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test"
libraryDependencies += "com.github.pathikrit" %% "better-files" % "2.17.1" % "test"
libraryDependencies += "org.scalameta" %% "scalameta" % "1.4.0" % "test"