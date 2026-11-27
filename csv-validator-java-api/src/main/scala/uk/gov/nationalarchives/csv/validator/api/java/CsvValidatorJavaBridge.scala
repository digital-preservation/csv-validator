/*
 * Copyright (c) 2013, The National Archives <digitalpreservation@nationalarchives.gov.uk>
 * https://www.nationalarchives.gov.uk
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package uk.gov.nationalarchives.csv.validator.api.java

import java.util.{ArrayList => JArrayList, List => JList}
import cats.data.{NonEmptyList, Validated, ValidatedNel}
import uk.gov.nationalarchives.csv.validator.{SchemaDefinitionError, ValidationError, ValidationWarning, FailMessage => SFailMessage, ProgressCallback => SProgressCallback}
import uk.gov.nationalarchives.csv.validator.Util._
import uk.gov.nationalarchives.csv.validator.api.CsvValidator.createValidator

import java.io.{Reader => JReader}
import uk.gov.nationalarchives.csv.validator.api.TextFile

import java.nio.charset.Charset
import java.nio.file.Paths

/**
 * Simple bridge from Java API to Scala API
 *
 * @author Adam Retter <adam.retter@googlemail.com>
 */
object CsvValidatorJavaBridge {

  @deprecated
  def validate(csvFile: String, csvEncoding: Charset, csvSchemaFile: String, csvSchemaEncoding: Charset, failFast: Boolean, pathSubstitutionsList: JList[Substitution], enforceCaseSensitivePathChecks: Boolean, trace: Boolean): JList[FailMessage] =
    collecting(callback => validateTextFile(csvFile, csvEncoding, true, csvSchemaFile, csvSchemaEncoding, false, failFast, pathSubstitutionsList, enforceCaseSensitivePathChecks, trace, None, callback = callback))

  @deprecated
  def validate(csvFile: String, csvEncoding: Charset, csvSchemaFile: String, csvSchemaEncoding: Charset, failFast: Boolean, pathSubstitutionsList: JList[Substitution], enforceCaseSensitivePathChecks: Boolean, trace: Boolean, progress: ProgressCallback): JList[FailMessage] = {
    val sProgressCallback = new SProgressCallback {
      override def update(complete: this.type#Percentage) = progress.update(complete)
    }
    collecting(callback => validateTextFile(csvFile, csvEncoding, true, csvSchemaFile, csvSchemaEncoding, false, failFast, pathSubstitutionsList, enforceCaseSensitivePathChecks, trace, Some(sProgressCallback), callback = callback))
  }

  @deprecated("use latest validate")
  def validate(csvFile: String, csvEncoding: Charset, validateCsvEncoding: Boolean, csvSchemaFile: String, csvSchemaEncoding: Charset, validateCsvSchemaEncoding: Boolean, failFast: Boolean, pathSubstitutionsList: JList[Substitution], enforceCaseSensitivePathChecks: Boolean, trace: Boolean): JList[FailMessage] =
    collecting(callback => validateTextFile(csvFile, csvEncoding, validateCsvEncoding, csvSchemaFile, csvSchemaEncoding, validateCsvSchemaEncoding, failFast, pathSubstitutionsList, enforceCaseSensitivePathChecks, trace, None, callback = callback))

  @deprecated("use latest validate")
  def validate(csvFile: String, csvEncoding: Charset, validateCsvEncoding: Boolean, csvSchemaFile: String, csvSchemaEncoding: Charset, validateCsvSchemaEncoding: Boolean, failFast: Boolean, pathSubstitutionsList: JList[Substitution], enforceCaseSensitivePathChecks: Boolean, trace: Boolean, progress: ProgressCallback): JList[FailMessage] = {
    val sProgressCallback = new SProgressCallback {
      override def update(complete: this.type#Percentage) = progress.update(complete)
    }
    collecting(callback => validateTextFile(csvFile, csvEncoding, validateCsvEncoding, csvSchemaFile, csvSchemaEncoding, validateCsvSchemaEncoding, failFast, pathSubstitutionsList, enforceCaseSensitivePathChecks, trace, Some(sProgressCallback), callback = callback))
  }

  def validate(validationRequest: ValidationRequest): ValidationResult = {
    val errors = collecting(validate(validationRequest, _))
    ValidationResult(errors, validationRequest)
  }

  def validate(validationRequest: ValidationRequest, callback: FailMessageCallback): Boolean = {
    val request = validationRequest
    validateTextFile(request.csvFile, request.csvEncoding, request.validateCsvEncoding, request.csvSchemaFile, request.csvSchemaEncoding, request.validateCsvSchemaEncoding, request.failFast, request.pathSubstitutionsList, request.enforceCaseSensitivePathChecks, request.trace, asScala(request.progress), request.skipFileChecks, request.maxCharsPerCellLimit, callback)
  }

  private def validateTextFile(csvFile: String, csvEncoding: Charset, validateCsvEncoding: Boolean, csvSchemaFile: String, csvSchemaEncoding: Charset, validateCsvSchemaEncoding: Boolean, failFast: Boolean, pathSubstitutionsList: JList[Substitution], enforceCaseSensitivePathChecks: Boolean, trace: Boolean, progress: Option[SProgressCallback], skipFileChecks: Boolean=false, maxCharsPerCellLimit: Int=4096, callback: FailMessageCallback): Boolean = {

    import scala.jdk.CollectionConverters._

    val pathSubs: List[(String,String)] = pathSubstitutionsList.asScala.map( x => (x.getFrom, x.getTo)).toList

    val csvTextFile = TextFile(Paths.get(csvFile), csvEncoding, validateCsvEncoding)
    val csvSchemaTextFile = TextFile(Paths.get(csvSchemaFile), csvSchemaEncoding, validateCsvSchemaEncoding)

    checkFilesReadable(csvTextFile.file :: csvSchemaTextFile.file :: Nil) match {
      case Validated.Invalid(errors) =>
        report(errors, callback)
        false

      case Validated.Valid(_) =>
        val validator = createValidator(failFast, pathSubs, enforceCaseSensitivePathChecks, trace, skipFileChecks, maxCharsPerCellLimit)
        validator.parseSchema(csvSchemaTextFile) match {

          case Validated.Invalid(errors) =>
            report(errors, callback)
            false

          case Validated.Valid(schema) =>
            validator.validateCsvFile(csvTextFile, schema, progress, rowCallback(callback))
        }
    }
  }

  @deprecated("use latest validate")
  def validate(csvData: JReader, csvSchema: JReader, failFast: Boolean, pathSubstitutionsList: JList[Substitution], enforceCaseSensitivePathChecks: Boolean, trace: Boolean): JList[FailMessage] =
    collecting(callback => validateReader(csvData, csvSchema, failFast, pathSubstitutionsList, enforceCaseSensitivePathChecks, trace, None, callback = callback))

  @deprecated("use latest validate")
  def validate(csvData: JReader, csvSchema: JReader, failFast: Boolean, pathSubstitutionsList: JList[Substitution], enforceCaseSensitivePathChecks: Boolean, trace: Boolean, progress: ProgressCallback): JList[FailMessage] = {
    val sProgressCallback = new SProgressCallback {
      override def update(complete: this.type#Percentage) = progress.update(complete)
    }
    collecting(callback => validateReader(csvData, csvSchema, failFast, pathSubstitutionsList, enforceCaseSensitivePathChecks, trace, Some(sProgressCallback), callback = callback))
  }

  def validate(validationRequest: ReaderValidationRequest): ReaderValidationResult = {
    val errors = collecting(validate(validationRequest, _))
    ReaderValidationResult(errors, validationRequest)
  }

  def validate(validationRequest: ReaderValidationRequest, callback: FailMessageCallback): Boolean = {
    val request = validationRequest
    validateReader(request.csvReader, request.csvSchemaReader, request.failFast, request.pathSubstitutionsList, request.enforceCaseSensitivePathChecks, request.trace, asScala(request.progress), request.skipFileChecks, request.maxCharsPerCellLimit, callback)
  }

  private def validateReader(csvData: JReader, csvSchema: JReader, failFast: Boolean, pathSubstitutionsList: JList[Substitution], enforceCaseSensitivePathChecks: Boolean, trace: Boolean, progress: Option[SProgressCallback], skipFileChecks: Boolean = false, maxCharsPerCell: Int = 4096, callback: FailMessageCallback): Boolean = {

    import scala.jdk.CollectionConverters._

    val pathSubs: List[(String,String)] = pathSubstitutionsList.asScala.map( x => (x.getFrom, x.getTo)).toList
    
    val validator = createValidator(failFast, pathSubs, enforceCaseSensitivePathChecks, trace, skipFileChecks, maxCharsPerCell)
    validator.parseSchema(csvSchema) match {

      case Validated.Invalid(errors) =>
        report(errors, callback)
        false

      case Validated.Valid(schema) =>
        validator.validateReader(csvData, schema, maxCharsPerCell, progress, rowCallback(callback))
    }
  }

  private def collecting(validation: FailMessageCallback => Boolean): JList[FailMessage] = {
    val messages = new JArrayList[FailMessage]
    validation(message => messages.add(message))
    messages
  }

  private def rowCallback(callback: FailMessageCallback): ValidatedNel[SFailMessage, Any] => Unit = {
    case Validated.Invalid(messages) => report(messages, callback)
    case _ =>
  }

  private def report(messages: NonEmptyList[SFailMessage], callback: FailMessageCallback): Unit =
    messages.toList.foreach(message => callback.onFailMessage(asJavaMessage(message)))

  private def asScala(progress: ProgressCallback): Option[SProgressCallback] =
    Option(progress).map(progress => new SProgressCallback {
      override def update(complete: this.type#Percentage) = progress.update(complete)
    })

  private def asJavaMessage(f: SFailMessage): FailMessage = f match {
    case SFailMessage(ValidationWarning, msg, lineNr, columnIdx) => new WarningMessage(msg, lineNr.getOrElse(-1), columnIdx.getOrElse(-1)).asInstanceOf[FailMessage]
    case SFailMessage(ValidationError, msg, lineNr, columnIdx) => new ErrorMessage(msg, lineNr.getOrElse(-1), columnIdx.getOrElse(-1)).asInstanceOf[FailMessage]
    case SFailMessage(SchemaDefinitionError, msg, lineNr, columnIdx) => new ErrorMessage(msg, lineNr.getOrElse(-1), columnIdx.getOrElse(-1)).asInstanceOf[FailMessage]
  }

   case class ValidationResult(errors: JList[FailMessage], validatorRequest: ValidationRequest) extends Result {
     override def getErrors: JList[FailMessage] = errors
   }
   case class ValidationRequest(csvFile: String, csvEncoding: Charset, validateCsvEncoding: Boolean, csvSchemaFile: String, csvSchemaEncoding: Charset, validateCsvSchemaEncoding: Boolean, failFast: Boolean, pathSubstitutionsList: JList[Substitution], enforceCaseSensitivePathChecks: Boolean, trace: Boolean, progress: ProgressCallback, skipFileChecks: Boolean, maxCharsPerCellLimit: Int)

   case class ReaderValidationResult(errors: JList[FailMessage], validatorRequest: ReaderValidationRequest) extends Result {
     override def getErrors: JList[FailMessage] = errors
   }
   case class ReaderValidationRequest(csvReader: JReader, csvSchemaReader: JReader, failFast: Boolean, pathSubstitutionsList: JList[Substitution], enforceCaseSensitivePathChecks: Boolean, trace: Boolean, progress: ProgressCallback, skipFileChecks: Boolean, maxCharsPerCellLimit: Int)
}
