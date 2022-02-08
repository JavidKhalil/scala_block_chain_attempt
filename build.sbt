name := "scalablockchainmini"

version := "0.1"

scalaVersion := "2.12.4"

lazy val akkaVersion = "2.6.16"

libraryDependencies ++= Seq(
  "org.apache.logging.log4j" % "log4j-core" % "2.7",
  "org.scalatest" %% "scalatest" % "3.2.10" % Test
)
