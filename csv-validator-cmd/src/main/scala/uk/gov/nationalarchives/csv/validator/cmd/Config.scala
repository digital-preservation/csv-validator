/**
 * Copyright (c) 2022, The National Archives <digitalpreservation@nationalarchives.gov.uk>
 * https://www.nationalarchives.gov.uk
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package uk.gov.nationalarchives.csv.validator
package cmd

import scopt.{Read, OptionParser}
import java.nio.charset.Charset
import java.nio.file.{Files, Path, Paths}
import uk.gov.nationalarchives.csv.validator.api.CsvValidator
import uk.gov.nationalarchives.csv.validator.api.CsvValidator.SubstitutePath
import java.util.jar.{Attributes, Manifest}
import scala.util.Using
import java.net.URL

case class Config(
  traceParser: Boolean = false,
  failFast: Boolean = false,
  substitutePaths: List[SubstitutePath] = List.empty,
  caseSensitivePaths: Boolean = false,
  showVersion: Boolean = false,
  csvPath: Path = Paths.get("."),
  csvEncoding: Charset = CsvValidator.DEFAULT_ENCODING,
  csvSchemaPath: Path = Paths.get("."),
  csvSchemaEncoding: Charset = CsvValidator.DEFAULT_ENCODING,
  disableUtf8Validation:Boolean = false,
  progressCallback: Option[ProgressCallback] = None
)

object Config {

  private def getShortVersion(): String = {
    extractFromManifest {
      attributes =>
        attributes.getValue("Implementation-Version")
    }.getOrElse("UNKNOWN")
  }

  private def extractFromManifest[T](extractor: Attributes => T): Option[T] = {
    val clazz = getClass()
    val className = clazz.getSimpleName + ".class"
    val classPath = clazz.getResource(className).toString()
    if (!classPath.startsWith("jar")) {
      None // Class not from JAR
    } else {
      val manifestPath = classPath.substring(0, classPath.lastIndexOf("!") + 1) + "/META-INF/MANIFEST.MF"
      Using(new URL(manifestPath).openStream()) { is =>
          val manifest = new Manifest(is)
          extractor(manifest.getMainAttributes)
      }.map(Some(_)).getOrElse(None)
    }
  }

  def fromArgsIO(args: Array[String]) = cats.effect.IO.blocking {
    fromArgs(args.toArray)
      .getOrElse(throw new IllegalArgumentException("cannot parse args"))
  }

  def fromArgs(args: Array[String]): Option[Config] = {

    implicit val pathRead: Read[Path] = Read.reads { Paths.get(_) }
    implicit val charsetRead: Read[Charset] = Read.reads { Charset.forName(_) }

    val parser = new OptionParser[Config]("validate") {
      head("CSV Validator - Command Line", getShortVersion())

      help("help") text("Prints this usage text")

      //opt[Boolean]("version") action { (x,c) => c.copy(showVersion = x) } text { "Display the version information" } //TODO would be nice if '--version' could be used without specifying CSV+CSVS paths etc //getLongVersion().map(x => s"${x._1}: ${x._2}").foreach(println(_))

      opt[Unit]('t', "trace-parser").optional().action {
        (_, c) => c.copy(traceParser = true)
      }.text("Prints a trace of the parser parse")

      opt[Boolean]('f', "fail-fast").optional().action {
        (x,c) => c.copy(failFast = x)
      }.text("Stops on the first validation error rather than reporting all errors")

      opt[SubstitutePath]('p', "path").optional().unbounded().action {
        (x,c) => c.copy(substitutePaths = c.substitutePaths :+ x)
      }.text("Allows you to substitute a file path (or part of) in the CSV for a different file path")

      opt[Boolean]('c', "case-sensitive-paths").optional().action {
        (x,c) => c.copy(caseSensitivePaths = x)
      }.text("Enforces case-sensitive file path checking. Useful when validating on case-insensitive filesystems like Windows NTFS")

      opt[Charset]('x', "csv-encoding").optional().action {
        (x,c) => c.copy(csvEncoding = x)
      } text("Defines the charset encoding used in the CSV file")

      opt[Charset]('y', "csv-schema-encoding").optional().action {
        (x,c) => c.copy(csvSchemaEncoding = x)
      }.text("Defines the charset encoding used in the CSV Schema file")

      opt[Unit]("disable-utf8-validation").optional().action {
        (_, c) => c.copy(disableUtf8Validation = true)
      }.text("Disable UTF-8 validation for CSV files.")

      opt[Unit]("show-progress").optional().action {
        (_, c) => c.copy(progressCallback = Some(CommandLineProgressCallback))
      }.text("Show progress")

      arg[Path]("<csv-path>").validate {
        case x if Files.exists(x) && Files.isReadable(x) => success
        case x => failure(s"Cannot access CSV file: ${x.toString}")
      }.action {
        (x,c) => c.copy(csvPath = x)
      }.text("The path to the CSV file to validate")

      arg[Path]("<csv-schema-path>").validate {
        x => if(Files.exists(x) && Files.isReadable(x)) success else failure(s"Cannot access CSV Schema file: ${x.toString}")
      }.action {
        (x,c) => c.copy(csvSchemaPath = x)
      }.text("The path to the CSV Schema file to use for validation")
    }

    //parse the command line arguments
    parser.parse(args, new Config())
  }
}
