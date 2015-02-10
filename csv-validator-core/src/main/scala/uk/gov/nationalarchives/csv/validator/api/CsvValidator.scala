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
import java.io.{Reader => JReader, File}
import java.nio.charset.{Charset => JCharset}

object CsvValidator {

  final val DEFAULT_ENCODING: JCharset = JCharset.forName("UTF-8")

  type PathFrom = String
  type PathTo = String
  type SubstitutePath = (PathFrom, PathTo)

  def createValidator(failFast: Boolean, pathSubstitutionsList: List[SubstitutePath],
           enforceCaseSensitivePathChecksSwitch: Boolean, integrityCheckFileColumn: Option[String]) = {
    if(failFast) {
      new CsvValidator with FailFastMetaDataValidator { 
        val pathSubstitutions = pathSubstitutionsList
        val enforceCaseSensitivePathChecks = enforceCaseSensitivePathChecksSwitch
        override val integrityCheckFilenameColumn = integrityCheckFileColumn
      }
    } else {
      new CsvValidator with AllErrorsMetaDataValidator {
        val pathSubstitutions = pathSubstitutionsList
        val enforceCaseSensitivePathChecks = enforceCaseSensitivePathChecksSwitch
        override val integrityCheckFilenameColumn = integrityCheckFileColumn
      }
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

  /**
   * If defined, specifies the name of the filename column to run the integrity check
   */
  val integrityCheckFilenameColumn: Option[String] = None

  val includeFolder = false
  
  /**
   * Validate the csvFile given as a parameter according to the schema, updating the progress
   * @param csvFile the CSV File 
   * @param csvSchema the CSV schema
   * @param progress the progress of the validation TODO the integrity check doesn't currently take into account the progress
   * @return Metadatavalidation of the validation
   */
  def validate(csvFile: TextFile, csvSchema: Schema, progress: Option[ProgressCallback]): MetaDataValidation[Any] = {
   
    val integrationValidation: MetaDataValidation[Any] = integrityCheckValidation(csvFile, csvSchema).getOrElse(true.successNel[FailMessage])

    val metadataValidation:MetaDataValidation[Any] = withReader(csvFile) {
      reader =>
        validateKnownRows(reader, csvSchema, progress.map(p => ProgressFor(countRows(csvFile), p)))
    }
    //TODO Combine in a better depending in the strategie FailFast or not
    List(integrationValidation, metadataValidation).sequence[MetaDataValidation, Any]
  }

  /**
   * Check if all the file under the content folder are listed in the metadata file listing
   * @param csvFile the CSV File
   * @param csvSchema the CSV schema
   * @return Option[MetaDataValidation] defined only the filename column is defined, and if so return the according metadata
   */
  def integrityCheckValidation(csvFile: TextFile, csvSchema: Schema): Option[MetaDataValidation[Any]] = {
    integrityCheckFilenameColumn.map {filenameColumn =>
      val columnDedinitionIds = csvSchema.columnDefinitions.map(_.id)
      if (columnDedinitionIds.exists(_ == filenameColumn)) {

        val filenameColumnIndex = columnDedinitionIds.indexOf(filenameColumn)
        val filnameColumnRules = csvSchema.columnDefinitions.apply(filenameColumnIndex)
        val allMetadataFilenames = withReader(csvFile) {
          reader =>
            getColumn(reader, csvSchema, filenameColumnIndex)
        }.map(new File(_).getName)
                csvFile.file.parent.map(_ / "content").map { contentPath =>
        val contentFile = new File(contentPath.toURI)

        scala.util.Try(Util.findAllFiles(includeFolder, contentFile)).map{ allContentFiles =>
          val allContentFilename = allContentFiles.map(_.getName)
          if (Util.containAll(allMetadataFilenames,allContentFilename.toList))
              true.successNel[FailMessage]
          else
              ErrorMessage(s"[Integrity Check], The file(s) ${allContentFilename.filterNot(allMetadataFilenames.toSet).mkString(" ")} " +
             s"are not listed in the metadata content under ${csvFile.file.parent}").failNel[Any]
          }.getOrElse {
            ErrorMessage(s"[Integrity Check], Cannot find the content folder under ${csvFile.file.parent}").failNel[Any]
          }
        }.getOrElse {
          ErrorMessage(s"[Integrity Check], Cannot find the content folder under ${csvFile.file.parent}").failNel[Any]
        }
      }
      else
        ErrorMessage(s"[Integrity Check], Cannot find the colunm $filenameColumn").failNel[Any]
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
