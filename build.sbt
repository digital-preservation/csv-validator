ThisBuild / scalaVersion := "2.13.7"

ThisBuild / scalacOptions += "-Xlint"

def testDeps = Seq(
  "org.specs2" %% "specs2-junit" % "4.14.1" % Test
)

lazy val core = (project in file("csv-validator-core"))
  .settings(
//    crossScalaVersions := Seq("2.12.15", "2.13.7", "3.1.1"),    
    libraryDependencies ++= Seq( 
      "org.scalaz" %% "scalaz-concurrent" % "7.2.34",
      "co.fs2" %% "fs2-io" % "3.2.5",
      "commons-io" % "commons-io" % "2.6", 
      "uk.gov.nationalarchives" % "utf8-validator" % "1.2", 
      "com.univocity" % "univocity-parsers" % "2.9.1",
      "org.scala-lang.modules" %% "scala-parser-combinators" % "2.1.0",
      "org.gfccollective" %% "gfc-semver" % "1.0.0", 
      "joda-time" % "joda-time" % "2.10.13",
    ) ++ testDeps
  )

lazy val cli = (project in file("csv-validator-cmd"))
  .settings(
    libraryDependencies ++= Seq(
      "com.github.scopt" %% "scopt" % "4.0.1", 
    ) ++ testDeps,
  )
  .dependsOn(core)
    
lazy val gui = (project in file("csv-validator-ui"))
  .settings(
    libraryDependencies ++= Seq(
      "org.scala-lang.modules" %% "scala-swing" % "3.0.0",
      "net.java.dev.designgridlayout" % "designgridlayout" % "1.11",
      "org.swinglabs.swingx" % "swingx-core" % "1.6.5-1"
    ),
  )
  .dependsOn(core, cli)

lazy val api = (project in file("csv-validator-java-api"))
  .dependsOn(core)
