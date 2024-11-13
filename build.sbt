val root = (project in file("."))
  .aggregate(csvValidatorCmd, csvValidatorCore)


lazy val csvValidatorCmd = (project in file("csv-validator-cmd"))
  .dependsOn(csvValidatorCore)
  .enablePlugins(ScalaJSPlugin)
  .settings(
    scalaJSUseMainModuleInitializer := true,
    scalaVersion := "2.13.12",
    libraryDependencies ++= Seq(
      "com.github.scopt" %%% "scopt" % "4.1.0",
      "org.typelevel" %%% "cats-core" % "2.11.0",
      "org.specs2" %%% "specs2-core" % "4.15.0" % Test,
      "org.specs2" %%% "specs2-common" % "4.15.0" % Test,
      "org.specs2" %%% "specs2-matcher" % "4.15.0" % Test
    )
  )


lazy val csvValidatorCore = (project in file("csv-validator-core"))
  .settings(
    scalaVersion := "2.13.12",
    libraryDependencies ++= Seq(
      "org.scala-lang.modules" %%% "scala-parser-combinators" % "2.3.0",
      "org.typelevel" %%% "cats-core" % "2.11.0",
      "org.typelevel" %%% "cats-kernel" % "2.11.0",
      "org.typelevel" %%% "cats-effect" % "3.5.0",      
      "org.typelevel" %%% "cats-effect-kernel" % "3.5.0",
      "com.nrinaudo" %%% "kantan.csv" % "0.7.0",
      "co.fs2" %%% "fs2-core" % "3.11.0",
      "co.fs2" %%% "fs2-io" % "3.11.0",
      "com.univocity" % "univocity-parsers" % "2.9.1",
      "io.kevinlee" %%% "just-semver-decver" % "1.0.0",
      "org.specs2" %%% "specs2-core" % "4.15.0" % Test,
      "org.specs2" %%% "specs2-common" % "4.15.0" % Test,
      "org.specs2" %%% "specs2-matcher" % "4.15.0" % Test
    )
  ).enablePlugins(ScalaJSPlugin)
