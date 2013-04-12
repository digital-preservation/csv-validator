import sbt._
import Keys._

object CsvValidatorBuild extends Build {
	lazy val root = Project(
		id = "csv-validator",
		base = file(".")) aggregate(parent, core)

	lazy val parent = Project(
		id = "csv-validator-parent",
		base = file("csv-validator-parent"))

	lazy val core = Project(
		id = "csv-validator-core",
		base = file("csv-validator-core"))

	
}
