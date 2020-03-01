name := "transactions"

version := "0.1"

scalaVersion := "2.13.1"

addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.11.0" cross CrossVersion.full)

libraryDependencies += "org.typelevel" %% "cats-effect" % "2.0.0"
libraryDependencies += "org.scalatest" %% "scalatest"   % "3.1.1" % "test"