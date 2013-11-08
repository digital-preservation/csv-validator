/*
 * Copyright (c) 2013, The National Archives digitalpreservation@nationalarchives.gov.uk
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package uk.gov.tna.dri.csv.validator.cmd

import scalaz.{Success => SuccessZ, Failure => FailureZ, _}
import scala.App
import uk.gov.tna.dri.csv.validator._
import uk.gov.tna.dri.csv.validator.api.CsvValidator.{SubstitutePath, createValidator}
import scalax.file.Path
import scopt.Read

object  SystemExits {
  val ValidCsv = 0
  val IncorrectArguments = 1
  val InvalidSchema = 2
  val InvalidCsv = 3
}

object CsvValidatorCmdApp extends App {

  val (exitMsg, exitCode) = run(args)
  println(exitMsg)
  System.exit(exitCode)

  case class Config(failFast: Boolean = false, substitutePaths: List[SubstitutePath] = List.empty[SubstitutePath], csvPath: Path = Path.fromString("."), csvSchemaPath: Path = Path.fromString("."))

  def run(args: Array[String]): (String,Int) = {

    implicit val pathRead: Read[Path] = Read.reads { Path.fromString(_) }

    val parser = new scopt.OptionParser[Config]("validate") {
        head("CSV Validator - Command Line")
        opt[Boolean]('f', "fail-fast") optional() action { (x,c) => c.copy(failFast = x) } text("Stops on the first validation error rather than reporting all errors")
        opt[SubstitutePath]('p', "path") optional() unbounded() action { (x,c) => c.copy(substitutePaths = c.substitutePaths :+ x) } text("Allows you to substitute a file path (or part of) in the CSV for a different file path")
        arg[Path]("<csv-path>") validate { x => if(x.exists && x.canRead) success else failure("Cannot access CSV file: " + x.path) } action { (x,c) => c.copy(csvPath = x) } text("The path to the CSV file to validate")
        arg[Path]("<csv-schema-path>") validate { x => if(x.exists && x.canRead) success else failure("Cannot access CSV Schema file: " + x.path) } action { (x,c) => c.copy(csvSchemaPath = x) } text("The path to the CSV Schema file to use for validation")
        help("help") text("Prints this usage text")
    }

    //parse the command line arguments
    parser.parse(args, new Config()) map {
      config =>
        processMetaData(config.csvPath, config.csvSchemaPath, config.failFast, config.substitutePaths)
    } getOrElse {
      //arguments are bad, usage message will have been displayed
      ("", SystemExits.IncorrectArguments)
    }
  }

  def processMetaData(metaDataFile: Path, schemaFile: Path, failFast: Boolean, pathSubstitutionsList: List[SubstitutePath] ): (String,Int) = {
    val validator = createValidator(failFast, pathSubstitutionsList)
    validator.parseSchema(schemaFile) match {
      case FailureZ(errors) => (prettyPrint(errors), SystemExits.InvalidSchema)
      case SuccessZ(schema) =>
        validator.validate(metaDataFile, schema) match {
          case FailureZ(errors) => (prettyPrint(errors), SystemExits.InvalidCsv)
          case SuccessZ(_) => ("PASS", SystemExits.ValidCsv)
        }
    }
  }

  private def prettyPrint(l: NonEmptyList[FailMessage]): String = l.list.map{i =>
    i match {
      case WarningMessage(err) => "Warning: " + err
      case ErrorMessage(err) =>   "Error:   " + err
      case SchemaMessage(err) =>  err
    }
  }.mkString(sys.props("line.separator"))
}