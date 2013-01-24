#!/bin/sh

###################################################################################################################################################
# 1a) Prepare for building an executable JAR by setting up sbt-assembly, which can build a (one) JAR that includes code plus all dependencies.
#     Note that any dependencies to be excluded can be done in several ways e.g. stating "provided" as part of a dependency. See
#     https://github.com/sbt/sbt-assembly
#
#     Within file ~/.sbt/plugins/build.sbt you must have sbt-assembly plugin:
#
#     resolvers += Resolver.url("artifactory", url("http://scalasbt.artifactoryonline.com/scalasbt/sbt-plugin-releases"))(Resolver.ivyStylePatterns)
#
#     addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "0.8.5")
#
# 1b) At the top of the project's build.sbt include the following:
#
#     import AssemblyKeys._
#
# 1c) Within project's build.sbt include:
#
#     assemblySettings
#
# 2)  Build an executable JAR for this script to run.
#
#     sbt assembly
#
# 3)  Don't forget to make this script runnable:
#
#     chmod +x validator.sh
#
# 4)  Run from command line as:
#
#     ./validator.sh <meta data file path> <schema file path>
###################################################################################################################################################

java -Xmx2048M -jar meta-data-validator.jar $1 $2
