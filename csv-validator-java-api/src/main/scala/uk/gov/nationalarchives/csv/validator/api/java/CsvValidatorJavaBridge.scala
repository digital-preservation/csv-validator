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
import uk.gov.nationalarchives.csv.validator.api.TextFile
import java.nio.charset.Charset

/**
 * Simple bridge from Java API to Scala API
 *
 * @author Adam Retter <adam.retter@googlemail.com>
 */
object CsvValidatorJavaBridge {

  def validate(csvFile: String, csvEncoding: Charset, csvSchemaFile: String, csvSchemaEncoding: Charset, failFast: Boolean, pathSubstitutionsList: JList[Substitution], enforceCaseSensitivePathChecks: Boolean): JList[FailMessage] =
    validate(csvFile, csvEncoding, csvSchemaFile, csvSchemaEncoding, failFast, pathSubstitutionsList, enforceCaseSensitivePathChecks, None)

  def validate(csvFile: String, csvEncoding: Charset, csvSchemaFile: String, csvSchemaEncoding: Charset, failFast: Boolean, pathSubstitutionsList: JList[Substitution], enforceCaseSensitivePathChecks: Boolean, progress: ProgressCallback): JList[FailMessage] = {
    val sProgressCallback = new SProgressCallback {
      override def update(complete: this.type#Percentage) = progress.update(complete)
    }
    validate(csvFile, csvEncoding, csvSchemaFile, csvSchemaEncoding, failFast, pathSubstitutionsList, enforceCaseSensitivePathChecks, Some(sProgressCallback))
  }

  //Todo handle integrity check
  private def validate(csvFile: String, csvEncoding: Charset, csvSchemaFile: String, csvSchemaEncoding: Charset, failFast: Boolean, pathSubstitutionsList: JList[Substitution], enforceCaseSensitivePathChecks: Boolean, progress: Option[SProgressCallback]): JList[FailMessage] = {

    import scala.collection.JavaConverters._

    val pathSubs: List[(String,String)] = pathSubstitutionsList.asScala.map( x => (x.getFrom, x.getTo)).toList

    val csvTextFile = TextFile(Path.fromString(csvFile), csvEncoding)
    val csvSchemaTextFile = TextFile(Path.fromString(csvSchemaFile), csvSchemaEncoding)

    checkFilesReadable(csvTextFile.file :: csvSchemaTextFile.file :: Nil) match {
      case FailureZ(errors) =>
        errors.list.map{ asJavaMessage(_) }.asJava

      case SuccessZ(_) =>
        val validator = createValidator(failFast, pathSubs, enforceCaseSensitivePathChecks, None)
        validator.parseSchema(csvSchemaTextFile) match {

          case FailureZ(errors) =>
            errors.list.map(asJavaMessage(_)).asJava

          case SuccessZ(schema) =>
            validator.validate(csvTextFile, schema, progress) match {
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

  //Todo handle integrity check
  private def validate(csvData: JReader, csvSchema: JReader, failFast: Boolean, pathSubstitutionsList: JList[Substitution],  enforceCaseSensitivePathChecks: Boolean, progress: Option[SProgressCallback]): JList[FailMessage] = {

    import scala.collection.JavaConverters._

    val pathSubs: List[(String,String)] = pathSubstitutionsList.asScala.map( x => (x.getFrom, x.getTo)).toList

    val validator = createValidator(failFast, pathSubs, enforceCaseSensitivePathChecks, None)
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