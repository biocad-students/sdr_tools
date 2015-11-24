name := "sdr_tools"

version := "1.0"

scalaVersion := "2.11.2"

libraryDependencies ++= Seq("io.spray" %%  "spray-json" % "1.3.2",
  "org.scalatest" % "scalatest_2.11" % "2.2.4" % "test",
  "org.scalautils" % "scalautils_2.11" % "2.1.5",
  //scalautils is included explicitly to use +- in production code (in qhull implementation)
  "com.typesafe.scala-logging" %% "scala-logging-slf4j" % "2.1.2",
  "org.slf4j" % "slf4j-api" % "1.7.5", "org.slf4j" % "slf4j-simple" % "1.7.5",
  "com.github.scopt" %% "scopt" % "3.3.0",
  "com.vspy" %% "mustache" % "1.2")

resolvers ++= Seq(
  "bintray-alexander_myltsev" at "http://dl.bintray.com/content/alexander-myltsev/maven",
  Resolver.sonatypeRepo("public"))
