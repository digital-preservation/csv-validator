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
import uk.gov.nationalarchives.csv.validator.schema._
import fs2._
import fs2.io._, file.Files
import java.nio.file.Path
import cats.syntax.all._
import uk.gov.nationalarchives.csv.validator.api.CsvValidator
import uk.gov.nationalarchives.csv.validator.metadata.Row
import uk.gov.nationalarchives.csv.validator.metadata.Cell
import cats.Show
import uk.gov.nationalarchives.csv.validator.schema.GlobalDirective

object CsvValidatorCmdApp3 extends IOApp {

  def run(args: List[String]): IO[ExitCode] = {

    val program = for {
      config <- Config.fromArgsIO(args.toArray)
      validator <- getValidator(config)
      s <- getSchema(validator, config)
      stats <- streamCSV[IO](config.csvPath)
        .zipWithIndex
        .through(maybeTail(!s.globalDirectives.contains(NoHeader())))
        .through(validateCSVLine[IO](validator, s))
        .evalTap{_.map(Console[IO].errorln(_)(showFailMessage)).sequence}
        .takeWhile(e => !e.contains(FailMessage.isError.isDefinedAt(_)) || !config.failFast)
        .compile
        .fold((0L,0L,0L)){case (acc,errors) => 
          val stats = (
            1L,
            errors.count(FailMessage.isError.isDefinedAt).toLong,
            errors.count(FailMessage.isWarning.isDefinedAt).toLong
          )
          (acc |+| stats)
        }
      _ <- IO.println("Rows Processed: " + stats._1)
      _ <- IO.println("Errors: " + stats._2)
      _ <- IO.println("Warnings: " + stats._3)            
    } yield (ExitCode.Success)

    program
      .handleErrorWith{
        case _: IllegalArgumentException => IO.pure(ExitCode(2))
        case unknown =>
          val msg = s"An error occurred: ${unknown.getLocalizedMessage()}"
          Console[IO].error(msg).as(ExitCode.Error)
      }

  }

  def maybeTail[F[_], A](doTail: Boolean): Pipe[F, A, A] = { in => 
    if (doTail) in.tail else in
  }

  def getValidator(config: Config): IO[CsvValidator] =
    IO.interruptible{
      createValidator(
        config.failFast,
        config.substitutePaths,
        config.caseSensitivePaths,
        config.traceParser
      )
    }

  def getSchema(validator: CsvValidator, config: Config): IO[Schema] =
    IO.interruptible{
      val schemaFile = TextFile(config.csvSchemaPath, config.csvSchemaEncoding)
      validator.parseSchema(schemaFile).toEither
    } flatMap {
      case Right(schema) =>
        IO.pure(schema)
      case Left(failures) =>
        failures.list.toList.map(Console[IO].error).sequence >>
        IO.raiseError(new Exception("Unable to read schema"))
    }
  

  def streamCSV[F[_]: Async](file: Path): Stream[F, List[String]] =
    Files[F].readAll(fs2.io.file.Path.fromNioPath(file))
      .through(text.utf8.decode)
      .through(text.lines)
      .map(_.split(",").toList) // TODO: Crude, replace with a better mechanism

  def validateCSVLine[F[_]: Async](
    validator: CsvValidator,
    schema: Schema
  ): Pipe[F, (List[String], Long), List[FailMessage]] = { stream =>
    stream.map{case (cells, idx) =>
      val row = Row.apply(cells.map(Cell), idx.toInt+1)
      validator.validateRow(row, schema, None)
        .toEither
        .left
        .toOption
        .fold(List.empty[FailMessage])(_.list.toList)
    }
  }

  implicit val showFailMessage: Show[FailMessage] = Show.show { 
    case FailMessage(ValidationWarning, err,_,_) => "Warning: " + err
    case FailMessage(ValidationError, err,_,_) =>   "Error:   " + err
    case FailMessage(SchemaDefinitionError, err,_,_) =>  err
  }

}
