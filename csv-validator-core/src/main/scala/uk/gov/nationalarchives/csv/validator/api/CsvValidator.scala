/**
 * Copyright (c) 2013, The National Archives <digitalpreservation@nationalarchives.gov.uk>
 * http://www.nationalarchives.gov.uk
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package uk.gov.nationalarchives.csv.validator.api

import uk.gov.nationalarchives.csv.validator.schema.{IntegrityCheck, Schema, SchemaParser}
import scalaz._, Scalaz._
import scalax.file.Path
import uk.gov.nationalarchives.csv.validator._
import java.io.{Reader => JReader, File}
import java.nio.charset.{Charset => JCharset}

object CsvValidator {

  final val DEFAULT_ENCODING: JCharset = JCharset.forName("UTF-8")

  type PathFrom = String
  type PathTo = String
  type SubstitutePath = (PathFrom, PathTo)

  def createValidator(failFast: Boolean, pathSubstitutionsList: List[SubstitutePath], enforceCaseSensitivePathChecksSwitch: Boolean, traceSwitch: Boolean) = {
    if(failFast) {
      new CsvValidator with FailFastMetaDataValidator { val pathSubstitutions = pathSubstitutionsList; val enforceCaseSensitivePathChecks = enforceCaseSensitivePathChecksSwitch; val trace = traceSwitch; }
    } else {
      new CsvValidator with AllErrorsMetaDataValidator { val pathSubstitutions = pathSubstitutionsList; val enforceCaseSensitivePathChecks = enforceCaseSensitivePathChecksSwitch; val trace = traceSwitch; }
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
case class TextFile(file: Path, encoding: JCharset = CsvValidator.DEFAULT_ENCODING)

trait CsvValidator extends SchemaParser {
  this: MetaDataValidator =>

  def validate(csvFile: TextFile, csvSchema: Schema, progress: Option[ProgressCallback]): MetaDataValidation[Any] = {
    val integrationValidation: MetaDataValidation[Any] = integrityCheckValidation(csvFile, csvSchema).getOrElse(true.successNel[FailMessage])

    val metadataValidation:MetaDataValidation[Any] = withReader(csvFile) {
      reader =>
        validateKnownRows(reader, csvSchema, progress.map(p => ProgressFor(countRows(csvFile), p)))
    }
    //TODO Combine in a better depending in the strategy FailFast or not
    List(integrationValidation, metadataValidation).sequence[MetaDataValidation, Any]
  }

  def integrityCheckValidation(csvFile: TextFile, csvSchema: Schema): Option[MetaDataValidation[Any]] = {
    val ic = csvSchema.globalDirectives.collectFirst{ case i @ IntegrityCheck(_, _) => i}
    ic.map {  integrityCheck =>
      val filenameColumn = integrityCheck.filepathColumn
      val filenameColumnIndex = csvSchema.columnDefinitions.map(x => x.id.value).indexOf(filenameColumn)
      val allMetadataFilenames = withReader(csvFile) {
        reader =>
          getColumn(reader, csvSchema, filenameColumnIndex)
      }.map(new File(_).getName)
      
      csvFile.file.parent.map(_ / "content").map { contentPath =>
        val contentFile = new File(contentPath.toURI)
        val includeFolder = integrityCheck.includeFolder

        scala.util.Try(Util.findAllFiles(includeFolder, contentFile)).map{ allContentFiles =>
          val allContentFilename = allContentFiles.map(_.getName)
          if (Util.containAll(allMetadataFilenames,allContentFilename.toList))
            true.successNel[FailMessage]
          else
            ErrorMessage(s"[Integrity Check], The file(s) ${Util.minus( allContentFilename, allMetadataFilenames.toSet).mkString(" ")} " +
              s"are not listed in the metadata content under ${csvFile.file.parent}").failNel[Any]
        }.getOrElse {
          ErrorMessage(s"[Integrity Check], Cannot find the content folder under ${csvFile.file.parent}").failNel[Any]
        }
      }.getOrElse {
        ErrorMessage(s"[Integrity Check], Cannot find the content folder under ${csvFile.file.parent}").failNel[Any]
      }
    }

  }

  def parseSchema(csvSchemaFile: TextFile): ValidationNel[FailMessage, Schema] = {
    withReader(csvSchemaFile) {
      reader =>
        parseAndValidate(reader)
    }
  }

  def parseSchema(csvSchema: JReader): ValidationNel[FailMessage, Schema] = parseAndValidate(csvSchema)
}
