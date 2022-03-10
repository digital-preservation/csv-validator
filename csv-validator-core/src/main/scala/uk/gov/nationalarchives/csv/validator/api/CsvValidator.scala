/**
 * Copyright (c) 2013, The National Archives <digitalpreservation@nationalarchives.gov.uk>
 * https://www.nationalarchives.gov.uk
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package uk.gov.nationalarchives.csv.validator.api

import uk.gov.nationalarchives.csv.validator.schema.{Schema, SchemaParser}
import scalaz._
import Scalaz._
import uk.gov.nationalarchives.csv.validator._

import java.io.{Reader => JReader}
import java.nio.charset.{Charset => JCharset}
import java.nio.file.Path

object CsvValidator {

  final val UTF_8: JCharset = JCharset.forName("UTF-8")

  final val DEFAULT_ENCODING = UTF_8

  type PathFrom = String
  type PathTo = String
  type SubstitutePath = (PathFrom, PathTo)

  def createValidator(failFast: Boolean, pathSubstitutionsList: List[SubstitutePath], enforceCaseSensitivePathChecksSwitch: Boolean, traceSwitch: Boolean) = {
    if(failFast) {
      new CsvValidator with FailFastMetaDataValidator { val pathSubstitutions = pathSubstitutionsList; val enforceCaseSensitivePathChecks = enforceCaseSensitivePathChecksSwitch; val trace = traceSwitch }
    } else {
      new CsvValidator with AllErrorsMetaDataValidator { val pathSubstitutions = pathSubstitutionsList; val enforceCaseSensitivePathChecks = enforceCaseSensitivePathChecksSwitch; val trace = traceSwitch }
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

  def validate(csvFile: TextFile, csvSchema: Schema, progress: Option[ProgressCallback]): MetaDataValidation[Any] = {

    val encodingValidationNel: MetaDataValidation[Any] = validateCsvFileEncoding(csvFile).getOrElse(true.successNel[FailMessage])

    val csvValidation = withReader(csvFile) {
      reader =>
        val totalRows = countRows(csvFile, csvSchema)
        validateKnownRows(reader, csvSchema, progress.map(p => {ProgressFor(totalRows, p)} )  )
    }
    List(encodingValidationNel,csvValidation).sequence[MetaDataValidation, Any]
  }


  def validateCsvFileEncoding(csvFile: TextFile): Option[MetaDataValidation[Any]] = csvFile match {
      // validateCsvFileEncoding(csvFile).getOrElse(true.successNel[FailMessage])

    case TextFile(_, _, false) => None
    case TextFile(_, encoding, _) if !encoding.equals(CsvValidator.UTF_8) => None
    case TextFile(file, _, true)  => Some(validateUtf8Encoding(file))
  }

  def parseSchema(csvSchemaFile: TextFile): ValidationNel[FailMessage, Schema] = {
    withReader(csvSchemaFile) {
      reader =>
        parseAndValidate(reader)
    }
  }

  def parseSchema(csvSchema: JReader): ValidationNel[FailMessage, Schema] = parseAndValidate(csvSchema)
}
