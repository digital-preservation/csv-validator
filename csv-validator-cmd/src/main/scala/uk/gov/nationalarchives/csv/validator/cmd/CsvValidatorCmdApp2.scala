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

import cats.effect._, std.Console
import api.CsvValidator.createValidator
import api.TextFile

object CsvValidatorCmdApp2 extends IOApp {

  def run(args: List[String]): IO[ExitCode] = {

    val configOpt = Config.fromArgs(args.toArray)

    configOpt match {
      case Some(config) =>
        val validator = createValidator(
          config.failFast,
          config.substitutePaths,
          config.caseSensitivePaths,
          config.traceParser
        )
        val schemaFile = TextFile(config.csvSchemaPath, config.csvSchemaEncoding)
        validator.parseSchema(schemaFile).toEither match {
          case Left(failures) =>
            val failuresMsg = prettyPrint(failures.list.toList)            
            Console[IO].error(failuresMsg).as(ExitCode(2))
          case Right(schema) =>

            val csvFile = TextFile(
              config.csvPath,
              config.csvEncoding,
              !config.disableUtf8Validation
            )

            validator.validate(csvFile, schema, config.progressCallback).toEither match {
              case Left(errorsZ) =>
                val errors = errorsZ.list.toList
                Console[IO].error(prettyPrint(errors)).as{
                  val isError = FailMessage.isError.isDefinedAt(_)
                  if (errors.exists(isError)) ExitCode(3) else ExitCode.Success
                }

              case Right(_) =>  IO.pure(ExitCode.Success)
            }
        }

      case None =>
        //arguments are bad, usage message will have been displayed
        IO.pure(ExitCode(1))
    }
  }

  private def prettyPrint(l: Iterable[FailMessage]): String = l.map { i =>
    i match {
      case FailMessage(ValidationWarning, err,_,_) => "Warning: " + err
      case FailMessage(ValidationError, err,_,_) =>   "Error:   " + err
      case FailMessage(SchemaDefinitionError, err,_,_) =>  err
    }
  }.toList.mkString(sys.props("line.separator"))
  
}
