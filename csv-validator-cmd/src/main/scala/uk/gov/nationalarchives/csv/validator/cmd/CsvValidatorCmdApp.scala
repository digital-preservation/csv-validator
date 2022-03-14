/**
 * Copyright (c) 2013, The National Archives <digitalpreservation@nationalarchives.gov.uk>
 * https://www.nationalarchives.gov.uk
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package uk.gov.nationalarchives.csv.validator
package cmd

import scalaz.{Failure => FailureZ, Success => SuccessZ, _}
import api.CsvValidator.SubstitutePath
import api.CsvValidator.createValidator
import api.TextFile

object SystemExitCodes extends Enumeration {
  type ExitCode = Int
  sealed abstract class SystemExitCode(val code: ExitCode)

  case object ValidCsv extends SystemExitCode(0)
  case object IncorrectArguments extends SystemExitCode(1)
  case object InvalidSchema extends SystemExitCode(2)
  case object InvalidCsv extends SystemExitCode(3)
}

object CsvValidatorCmdApp extends App {

  type ExitMessage = String
  type ExitStatus = (ExitMessage, SystemExitCodes.SystemExitCode)

  val (exitMessage, systemExitCode) = run(args)
  println(exitMessage)
  System.exit(systemExitCode.code)

  def run(args: Array[String]): ExitStatus = {

    //parse the command line arguments
    Config.fromArgs(args) match {
      case Some(config) =>
        validate(
          TextFile(config.csvPath, config.csvEncoding, !config.disableUtf8Validation),
          TextFile(config.csvSchemaPath, config.csvSchemaEncoding),
          config.failFast,
          config.substitutePaths,
          config.caseSensitivePaths,
          config.traceParser,
          config.progressCallback
        )
      case None => 
      //arguments are bad, usage message will have been displayed
      ("", SystemExitCodes.IncorrectArguments)
    }
  }

  def validate(
    csvFile: TextFile,
    schemaFile: TextFile,
    failFast: Boolean,
    pathSubstitutionsList: List[SubstitutePath],
    enforceCaseSensitivePathChecks: Boolean,
    trace: Boolean,
    progress: Option[ProgressCallback]
  ): ExitStatus = {
    val validator = createValidator(failFast, pathSubstitutionsList, enforceCaseSensitivePathChecks, trace)
    validator.parseSchema(schemaFile) match {
      case FailureZ(errors) => (prettyPrint(errors), SystemExitCodes.InvalidSchema)
      case SuccessZ(schema) =>
        validator.validate(csvFile, schema, progress) match {
          case FailureZ(failures) =>
            val failuresMsg = prettyPrint(failures)
            if(containsError(failures))  //checks for just warnings to determine exit code
              (failuresMsg + EOL + "FAIL",
                SystemExitCodes.InvalidCsv)
            else
              (failuresMsg + EOL + "PASS", //just warnings!
                SystemExitCodes.ValidCsv)

          case SuccessZ(_) => ("PASS", SystemExitCodes.ValidCsv)
        }
    }
  }

  private def containsError(l: NonEmptyList[FailMessage]) : Boolean = {
    l.list.toList.exists {
      case FailMessage(ValidationError, _, _, _) => true
      case _ => false
    }
  }

  private def prettyPrint(l: NonEmptyList[FailMessage]): String = l.list.map { i =>
    i match {
      case FailMessage(ValidationWarning, err,_,_) => "Warning: " + err
      case FailMessage(ValidationError, err,_,_) =>   "Error:   " + err
      case FailMessage(SchemaDefinitionError, err,_,_) =>  err
    }
  }.toList.mkString(sys.props("line.separator"))
}
