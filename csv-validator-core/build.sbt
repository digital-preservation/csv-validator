import AssemblyKeys._

name := "csv-validator-core"

//TODO remove in favour of csv-validator-parent/build.sbt
scalaVersion in ThisBuild := "2.10.0"

parallelExecution in Test := true

publishMavenStyle := true

assemblySettings

//test in assembly := {}  // skip the 'sbt test' section

jarName in assembly := "meta-data-validator.jar"

mainClass in assembly := Some( "uk.gov.tna.dri.validator.MetaDataValidatorCommandLineApp" )

libraryDependencies ++= Seq(
  "org.specs2" %% "specs2" % "1.12.3" % "test",
  "org.scalatest" %% "scalatest" % "1.9.1" % "test"
)

libraryDependencies ++= Seq(
  "net.sf.opencsv" % "opencsv" % "2.3",
  "org.scalaz" %% "scalaz-core" % "6.0.4",
  "com.github.scala-incubator.io" % "scala-io-core_2.10" % "0.4.2",
  "com.github.scala-incubator.io" % "scala-io-file_2.10" % "0.4.2",
  "joda-time" % "joda-time" % "2.1",
  "org.joda" % "joda-convert" % "1.2"
)
