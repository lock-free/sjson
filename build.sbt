name := "sjson"
organization := "io.github.lock-free"
version := "0.2.2"
scalaVersion := "2.12.4"

useGpg := true 
parallelExecution in Test := true

publishTo := sonatypePublishTo.value

libraryDependencies ++= Seq(
  "org.scala-lang" % "scala-reflect" % scalaVersion.value,
  // test suite
  "org.scalatest" %% "scalatest" % "3.0.1" % Test
)
