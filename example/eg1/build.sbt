
name := "sjson-eg1"
organization := "io.github.lock-free"
version := "0.0.1"
scalaVersion := "2.12.4"

libraryDependencies ++= Seq(
  "io.github.lock-free" %% "sjson" % "0.1.2",
  // test suite
  "org.scalatest" %% "scalatest" % "3.0.1" % Test
)
