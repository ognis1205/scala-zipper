// Specifies Project's Name.
name := "Scala Implementation of Pseudo-Mutable Data Types with Zipper Monad."

// Specifies the Organization which Manages this Project.
organization := "jp.supership.search"

// Specifies the Version of this Project.
version := "0.0.1"

// Resolves Repository Name and its URL.
resolvers ++= Seq(
  "scalaz-bintray" at "http://dl.bintray.com/scalaz/releases",
  "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/")

// Specifies Library Dependencies.
libraryDependencies ++= Seq(
  "org.json4s" %% "json4s-native" % "3.3.0",
  "junit" % "junit" % "4.11" % "test")
