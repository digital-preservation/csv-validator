/**
 * Copyright (c) 2013, The National Archives <digitalpreservation@nationalarchives.gov.uk>
 * http://www.nationalarchives.gov.uk
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package uk.gov.nationalarchives.csv.validator.api

import uk.gov.nationalarchives.csv.validator.schema.{Schema, SchemaParser}
import scalaz._, Scalaz._
import scalax.file.Path
import uk.gov.nationalarchives.csv.validator._
import java.io.{Reader => JReader}

object CsvValidator {

  type PathFrom = String
  type PathTo = String
  type SubstitutePath = (PathFrom, PathTo)

  def createValidator(failFast: Boolean, pathSubstitutionsList: List[SubstitutePath], enforceCaseSensitivePathChecksSwitch: Boolean) = {
    if(failFast) {
      new CsvValidator with FailFastMetaDataValidator { val pathSubstitutions = pathSubstitutionsList; val enforceCaseSensitivePathChecks = enforceCaseSensitivePathChecksSwitch }
    } else {
      new CsvValidator with AllErrorsMetaDataValidator { val pathSubstitutions = pathSubstitutionsList; val enforceCaseSensitivePathChecks = enforceCaseSensitivePathChecksSwitch }
    }
  }
}

trait CsvValidator extends SchemaParser {
  this: MetaDataValidator =>

  def validate(metaDataFile: Path, schema: Schema, progress: Option[ProgressCallback]): MetaDataValidation[Any] = {
    withReader(metaDataFile) {
      reader =>
        validateKnownRows(reader, schema, progress.map(p => ProgressFor(countRows(metaDataFile), p)))
    }
  }

  def parseSchema(schemaFilePath: Path): ValidationNel[FailMessage, Schema] = {
    withReader(schemaFilePath) {
      reader =>
        parseAndValidate(reader)
    }
  }

  def parseSchema(schema: JReader): ValidationNel[FailMessage, Schema] = parseAndValidate(schema)
}
