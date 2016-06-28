name := "exercies"

scalaVersion := "2.11.7"

scalacOptions ++= Seq("-deprecation")

// grading libraries
libraryDependencies += "junit" % "junit" % "4.10" % "test"
libraryDependencies += "ch.epfl.lamp" % "scala-grading-runtime_2.11" % "0.3"


