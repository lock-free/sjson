name := "sjson"
organization := "com.shopee.idata"
version := "0.1-SNAPSHOT"
scalaVersion := "2.12.4"

parallelExecution in Test := true

libraryDependencies ++= Seq(
  "org.scala-lang" % "scala-reflect" % scalaVersion.value,
  // test suite
  "org.scalatest" %% "scalatest" % "3.0.1" % Test
)
