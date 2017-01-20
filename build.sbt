lazy val root = (project in file("."))
  .settings(
    name := "dot-lang",
    scalaVersion := "2.11.8"
  )

libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.1"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test"
