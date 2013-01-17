name := "csv-validator-core"

version := "1.0"

scalaVersion := "2.10.0"

publishTo := Some("Artifactory Realm" at "http://wb-d-tfs2.web.local:8081/artifactory/libs-release-local")

credentials += Credentials("Artifactory Realm", "wb-d-tfs2.web.local", "admin", "Swansong7")

publishMavenStyle := true

resolvers ++= Seq("snapshots" at "http://oss.sonatype.org/content/repositories/snapshots",
                  "releases"  at "http://oss.sonatype.org/content/repositories/releases")

libraryDependencies ++= Seq(
  "org.specs2" %% "specs2" % "1.12.3" % "test"
)


