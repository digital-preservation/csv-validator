/**
 * Copyright (c) 2013, The National Archives <digitalpreservation@nationalarchives.gov.uk>
 * http://www.nationalarchives.gov.uk
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package uk.gov.nationalarchives.csv.validator.api.java

import java.util.{List => JList, ArrayList => JArrayList}
import scalax.file.Path
import scalaz.{Success => SuccessZ, Failure => FailureZ, _}
import uk.gov.nationalarchives.csv.validator.{ProgressCallback => SProgressCallback, FailMessage => SFailMessage, WarningMessage => SWarningMessage, ErrorMessage => SErrorMessage, SchemaMessage => SSchemaMessage}
import uk.gov.nationalarchives.csv.validator.Util._
import uk.gov.nationalarchives.csv.validator.api.CsvValidator.createValidator
import java.io.{Reader => JReader}

/**
 * Simple bridge from Java API to Scala API
 */
object CsvValidatorJavaBridge {

  def validate(csvDataFile: String, csvSchemaFile: String, failFast: Boolean, pathSubstitutionsList: JList[Substitution], enforceCaseSensitivePathChecks: Boolean): JList[FailMessage] =
    validate(csvDataFile, csvSchemaFile, failFast, pathSubstitutionsList, enforceCaseSensitivePathChecks, None)

  def validate(csvDataFile: String, csvSchemaFile: String, failFast: Boolean, pathSubstitutionsList: JList[Substitution], enforceCaseSensitivePathChecks: Boolean, progress: ProgressCallback): JList[FailMessage] = {
    val sProgressCallback = new SProgressCallback {
      override def update(complete: this.type#Percentage) = progress.update(complete)
    }
    validate(csvDataFile, csvSchemaFile, failFast, pathSubstitutionsList, enforceCaseSensitivePathChecks, Some(sProgressCallback))
  }

  private def validate(csvDataFile: String, csvSchemaFile: String, failFast: Boolean, pathSubstitutionsList: JList[Substitution], enforceCaseSensitivePathChecks: Boolean, progress: Option[SProgressCallback]): JList[FailMessage] = {

    import scala.collection.JavaConverters._

    val pathSubs: List[(String,String)] = pathSubstitutionsList.asScala.map( x => (x.getFrom, x.getTo)).toList

    val pMetaDataFile = Path.fromString(csvDataFile)
    val pSchemaFile = Path.fromString(csvSchemaFile)

    checkFilesReadable(pMetaDataFile :: pSchemaFile :: Nil) match {
      case FailureZ(errors) =>
        errors.list.map{ asJavaMessage(_) }.asJava

      case SuccessZ(_) =>
        val validator = createValidator(failFast, pathSubs, enforceCaseSensitivePathChecks)
        validator.parseSchema(pSchemaFile) match {

          case FailureZ(errors) =>
            errors.list.map(asJavaMessage(_)).asJava

          case SuccessZ(schema) =>
            validator.validate(pMetaDataFile, schema, progress) match {
              case FailureZ(errors) => errors.list.map(asJavaMessage(_)).asJava
              case SuccessZ(_) => new JArrayList[FailMessage]
            }
        }
    }
  }

  def validate(csvData: JReader, csvSchema: JReader, failFast: Boolean, pathSubstitutionsList: JList[Substitution], enforceCaseSensitivePathChecks: Boolean): JList[FailMessage] =
    validate(csvData, csvSchema, failFast, pathSubstitutionsList, enforceCaseSensitivePathChecks, None)

  def validate(csvData: JReader, csvSchema: JReader, failFast: Boolean, pathSubstitutionsList: JList[Substitution], enforceCaseSensitivePathChecks: Boolean, progress: ProgressCallback): JList[FailMessage] = {
    val sProgressCallback = new SProgressCallback {
      override def update(complete: this.type#Percentage) = progress.update(complete)
    }
    validate(csvData, csvSchema, failFast, pathSubstitutionsList, enforceCaseSensitivePathChecks, Some(sProgressCallback))
  }

  private def validate(csvData: JReader, csvSchema: JReader, failFast: Boolean, pathSubstitutionsList: JList[Substitution],  enforceCaseSensitivePathChecks: Boolean, progress: Option[SProgressCallback]): JList[FailMessage] = {

    import scala.collection.JavaConverters._

    val pathSubs: List[(String,String)] = pathSubstitutionsList.asScala.map( x => (x.getFrom, x.getTo)).toList

    val validator = createValidator(failFast, pathSubs, enforceCaseSensitivePathChecks)
    validator.parseSchema(csvSchema) match {

      case FailureZ(errors) =>
        errors.list.map(asJavaMessage(_)).asJava

      case SuccessZ(schema) =>
        validator.validate(csvData, schema, progress) match {
          case FailureZ(errors) => errors.list.map(asJavaMessage(_)).asJava
          case SuccessZ(_) => new JArrayList[FailMessage]
        }
    }
  }

  private def asJavaMessage(f: SFailMessage): FailMessage = f match {
    case SWarningMessage(msg) => new WarningMessage(msg).asInstanceOf[FailMessage]
    case SErrorMessage(msg) => new ErrorMessage(msg).asInstanceOf[FailMessage]
    case SSchemaMessage(msg) => new ErrorMessage(msg).asInstanceOf[FailMessage]
  }
}