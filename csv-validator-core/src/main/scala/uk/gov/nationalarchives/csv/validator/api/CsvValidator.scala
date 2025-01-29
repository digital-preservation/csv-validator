/*
 * Copyright (c) 2013, The National Archives <digitalpreservation@nationalarchives.gov.uk>
 * https://www.nationalarchives.gov.uk
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package uk.gov.nationalarchives.csv.validator.api

import cats.data.{Chain, Validated, ValidatedNel}
import cats.implicits._
import com.univocity.parsers.csv.{CsvParser, CsvParserSettings}
import uk.gov.nationalarchives.csv.validator._
import uk.gov.nationalarchives.csv.validator.schema.{Quoted, Schema, SchemaParser, Separator}

import java.io.{Reader => JReader}
import java.nio.charset.{Charset => JCharset}
import java.nio.file.Path
import scala.jdk.CollectionConverters._
import scala.util.Try

object CsvValidator {

  final val UTF_8: JCharset = JCharset.forName("UTF-8")

  final val DEFAULT_ENCODING = UTF_8

  type PathFrom = String
  type PathTo = String
  type SubstitutePath = (PathFrom, PathTo)

  def createValidator(failFast: Boolean, pathSubstitutionsList: List[SubstitutePath], enforceCaseSensitivePathChecksSwitch: Boolean, traceSwitch: Boolean, skipFileChecksSwitch: Boolean, maxCharsPerCellLimit: Int) = {
    if(failFast) {
      new CsvValidator with FailFastMetaDataValidator { val pathSubstitutions = pathSubstitutionsList; val enforceCaseSensitivePathChecks = enforceCaseSensitivePathChecksSwitch; val trace = traceSwitch; val skipFileChecks = skipFileChecksSwitch; val maxCharsPerCell = maxCharsPerCellLimit}
    } else {
      new CsvValidator with AllErrorsMetaDataValidator { val pathSubstitutions = pathSubstitutionsList; val enforceCaseSensitivePathChecks = enforceCaseSensitivePathChecksSwitch; val trace = traceSwitch; val skipFileChecks = skipFileChecksSwitch; val maxCharsPerCell = maxCharsPerCellLimit }
    }
  }
}

/**
  * Represent a Text file on disk
  * that has both a path and a specific
  * encoding.
  *
  * If no encoding is specified, then UTF-8 will
  * be assumed.
  */
case class TextFile(file: Path, encoding: JCharset = CsvValidator.DEFAULT_ENCODING, validateEncoding: Boolean = true)

trait CsvValidator extends SchemaParser {
  this: MetaDataValidator =>

  @deprecated("use validateReader or validateCsvFile")
  def validate(
    csvFile: TextFile,
    schema: Schema,
    progress: Option[ProgressCallback]
  ): MetaDataValidation[Any] = {
    var results: Chain[List[FailMessage]] = Chain.empty
    validateCsvFile(
      csvFile,
      schema,
      maxCharsPerCell,
      progress,
      {
        case Validated.Invalid(x) =>
          results = results :+ x.toList
        case _ =>
      }
    )
    results.toList.flatten.toNel match {
      case None => ().valid
      case Some(errors) => Validated.invalid(errors)
    }
  }
  
  
  
  def loadCsvFile(csvFile: TextFile, csvSchemaFile: TextFile, maxCharsPerCell: Int): List[Array[String]] = {
    parseSchema(csvSchemaFile) match {
      case Validated.Valid(schema) =>
        withReader(csvFile) { reader =>
          createCsvParser(schema, maxCharsPerCell).parseAll(reader)
        }.asScala.toList
      case Validated.Invalid(_) => Nil
    }
  }
  
  def validateCsvFile(
    csvFile: TextFile,
    csvSchema: Schema,
    maxCharsPerCell: Int,
    progress: Option[ProgressCallback],
    rowCallback: MetaDataValidation[Any] => Unit
  ): Boolean = {

    val encodingValidationNel: MetaDataValidation[Any] = validateCsvFileEncoding(csvFile).getOrElse(true.validNel[FailMessage])
    rowCallback(encodingValidationNel)

    val csvValidation = withReader(csvFile) {
      reader =>
        val totalRows = countRows(csvFile, csvSchema)
        validateKnownRows(reader, csvSchema, maxCharsPerCell, progress.map(p => {ProgressFor(totalRows, p)} ), rowCallback)
    }
    encodingValidationNel.isValid && csvValidation
  }


  def validateCsvFileEncoding(csvFile: TextFile): Option[MetaDataValidation[Any]] = csvFile match {
      // validateCsvFileEncoding(csvFile).getOrElse(true.validNel[FailMessage])

    case TextFile(_, _, false) => None
    case TextFile(_, encoding, _) if !encoding.equals(CsvValidator.UTF_8) => None
    case TextFile(file, _, true)  => Some(validateUtf8Encoding(file))
  }

  def parseSchema(csvSchemaFile: TextFile): ValidatedNel[FailMessage, Schema] = {
    withReader(csvSchemaFile) {
      reader =>
        parseAndValidate(reader)
    }
  }

  def parseSchema(csvSchema: JReader): ValidatedNel[FailMessage, Schema] = parseAndValidate(csvSchema)
}
