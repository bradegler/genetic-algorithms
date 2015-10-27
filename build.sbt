name := """ai"""

version := "1.0"

scalaVersion := "2.11.6"

val akkaVersion = "2.3.9"

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature")

libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-actor" % akkaVersion,
  "com.typesafe.akka" %% "akka-testkit" % akkaVersion % "test",
  "org.specs2" %% "specs2-core" % "2.4.15" % "test"
  )
