name := "ir"

organization := "ir"

version := "0.0.1"

scalaVersion := "2.11.5"

libraryDependencies ++= Seq(
	"org.scalatest" %% "scalatest" % "2.2.4" % "test",
	"org.apache.pdfbox" % "pdfbox" % "1.8.8",
	"junit" % "junit" % "4.11"
)

initialCommands := "import ir._"
