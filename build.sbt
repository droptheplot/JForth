name := "JForth"

version := "0.1"

scalaVersion := "2.12.8"

scalacOptions += "-Ypartial-unification"

libraryDependencies += "org.typelevel" %% "cats-core" % "1.5.0"
libraryDependencies += "org.typelevel" %% "cats-effect" % "1.3.1"

libraryDependencies += "org.ow2.asm" % "asm" % "7.1"

libraryDependencies += "com.lihaoyi" %% "fastparse" % "2.1.3"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % "test"

libraryDependencies += "com.github.scopt" %% "scopt" % "4.0.0-RC2"
