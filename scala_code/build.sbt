name := "sdr_tools"

version := "1.0"

scalaVersion := "2.11.2"

libraryDependencies += "io.spray" %%  "spray-json" % "1.3.2"

libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.4" % "test"

libraryDependencies += "com.typesafe.scala-logging" %% "scala-logging-slf4j" % "2.1.2"

libraryDependencies ++= Seq("org.slf4j" % "slf4j-api" % "1.7.5",
    "org.slf4j" % "slf4j-simple" % "1.7.5")

libraryDependencies += "com.github.scopt" %% "scopt" % "3.3.0"

resolvers += "bintray-alexander_myltsev" at "http://dl.bintray.com/content/alexander-myltsev/maven"

libraryDependencies += "com.vspy" %% "mustache" % "1.2"

resolvers += Resolver.sonatypeRepo("public")
