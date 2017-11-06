resolvers += "Artima Maven Repository" at "http://repo.artima.com/releases"

name := "functional-programming-in-scala"

version := "0.1"

scalaVersion := "2.12.4"

libraryDependencies ++= Seq(
  "org.scalactic" %% "scalactic" % "3.0.4",
  "org.scalatest" %% "scalatest" % "3.0.4" % "test")
