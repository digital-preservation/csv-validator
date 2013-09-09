import AssemblyKeys._

name := "csv-validator-ui"

version := "1.0"

organization := "uk.gov.nationalarchives"

//TODO remove in favour of csv-validator-parent/build.sbt
scalaVersion in ThisBuild := "2.10.2"

publishMavenStyle := true

assemblySettings

jarName in assembly := "csv-validator-ui.jar"

mainClass in assembly := Some( "uk.gov.tna.dri.ui.CsvValidatorUi" )

libraryDependencies ++= Seq(
    "org.scala-lang" % "scala-swing" % "2.10.0",
    libraryDependencies += "com.jsuereth" %% "scala-arm" % "1.3"
    "net.java.dev.designgridlayout" % "designgridlayout" % "1.10",
    "org.swinglabs.swingx" % "swingx-core" % "1.6.5-1",
    "uk.gov.nationalarchives" %% "csv-validator-core" % "1.0"
)
