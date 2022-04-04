// ==================
// Main Configuration
// ==================

ThisBuild / scalaVersion := "2.13.7"

ThisBuild / scalacOptions += "-Xlint"

def testDeps = Seq(
  "org.specs2" %% "specs2-junit" % "4.14.1" % Test
)

lazy val core = (project in file("csv-validator-core"))
  .settings(
    crossScalaVersions := Seq(
      "2.12.15",
      "2.13.7",
      // "3.1.1" // needs more rework, suggest removal of unneeded libraries first and refactor of code
    ),
    libraryDependencies ++= Seq( 
      "org.scalaz" %% "scalaz-concurrent" % "7.2.34",
      "co.fs2" %% "fs2-io" % "3.2.5",
      "commons-io" % "commons-io" % "2.6", 
      "uk.gov.nationalarchives" % "utf8-validator" % "1.2", 
      "com.univocity" % "univocity-parsers" % "2.9.1",
      "org.scala-lang.modules" %% "scala-parser-combinators" % "2.1.0",
      ("org.gfccollective" %% "gfc-semver" % "1.0.0").cross(CrossVersion.for3Use2_13), 
      "joda-time" % "joda-time" % "2.10.13",
      "org.scala-lang.modules" %% "scala-collection-compat" % "2.7.0"
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


// ===================================
// Publishing & metadata configuration
// ===================================

import com.typesafe.sbt.pgp.PgpKeys.{publishSignedConfiguration, publishLocalSignedConfiguration}

scmInfo := Some(
  ScmInfo(
    url("https://github.com/ltbs/uniform-scala"),
    "scm:git@github.com:ltbs/uniform-scala.git"
  )
)

publishTo := {
  sonatypePublishToBundle.value
}
publishConfiguration := publishConfiguration.value.withOverwrite(isSnapshot.value)
publishSignedConfiguration := com.typesafe.sbt.pgp.PgpKeys.publishSignedConfiguration.value.withOverwrite(isSnapshot.value)
publishLocalConfiguration := publishLocalConfiguration.value.withOverwrite(isSnapshot.value)
publishLocalSignedConfiguration := com.typesafe.sbt.pgp.PgpKeys.publishLocalSignedConfiguration.value.withOverwrite(isSnapshot.value)
useGpg := true

pomExtra := (
  <modelVersion>4.0.0</modelVersion>
  <description>Validation tool for CSV and CSV Schema files</description>
  <url>https://digital-preservation.github.io/csv-validator/</url>
  <inceptionYear>2013</inceptionYear>

  <organization>
      <name>The National Archives</name>
      <url>https://www.nationalarchives.gov.uk</url>
  </organization>
  <developers>
      <developer>
          <name>David Ainslie</name>
      </developer>
      <developer>
          <name>Jim Collins</name>
      </developer>
      <developer>
          <name>Andy Hicks</name>
      </developer>
      <developer>
          <name>Ben Parker</name>
      </developer>
      <developer>
          <name>Adam Retter</name>
      </developer>
      <developer>
          <name>Laura Damian</name>
      </developer>
      <developer>
          <name>Radek Hubner</name>
      </developer>
      <developer>
          <name>Valy Diarrassouba</name>
      </developer>
  </developers>
  <contributors>
      <contributor>
          <name>Nicola Welch</name>
      </contributor>
      <contributor>
          <name>Alex Green</name>
      </contributor>
      <contributor>
          <name>David Underdown</name>
      </contributor>
      <contributor>
          <name>Eric Torreborre</name>
      </contributor>
  </contributors>

  <licenses>
      <license>
          <name>The Mozilla Public License Version 2.0</name>
          <url>http://opensource.org/licenses/MPL-2.0</url>
          <distribution>repo</distribution>
      </license>
  </licenses>)
