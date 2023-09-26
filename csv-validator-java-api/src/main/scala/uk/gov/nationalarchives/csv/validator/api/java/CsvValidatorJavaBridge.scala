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
import cats.data.Validated
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
    validate(csvFile, csvEncoding, true, csvSchemaFile, csvSchemaEncoding, false, failFast, pathSubstitutionsList, enforceCaseSensitivePathChecks, trace, None)

  @deprecated
  def validate(csvFile: String, csvEncoding: Charset, csvSchemaFile: String, csvSchemaEncoding: Charset, failFast: Boolean, pathSubstitutionsList: JList[Substitution], enforceCaseSensitivePathChecks: Boolean, trace: Boolean, progress: ProgressCallback): JList[FailMessage] = {
    val sProgressCallback = new SProgressCallback {
      override def update(complete: this.type#Percentage) = progress.update(complete)
    }
    validate(csvFile, csvEncoding, true, csvSchemaFile, csvSchemaEncoding, false, failFast, pathSubstitutionsList, enforceCaseSensitivePathChecks, trace, Some(sProgressCallback))
  }

  def validate(csvFile: String, csvEncoding: Charset, validateCsvEncoding: Boolean, csvSchemaFile: String, csvSchemaEncoding: Charset, validateCsvSchemaEncoding: Boolean, failFast: Boolean, pathSubstitutionsList: JList[Substitution], enforceCaseSensitivePathChecks: Boolean, trace: Boolean): JList[FailMessage] =
    validate(csvFile, csvEncoding, validateCsvEncoding, csvSchemaFile, csvSchemaEncoding, validateCsvSchemaEncoding, failFast, pathSubstitutionsList, enforceCaseSensitivePathChecks, trace, None)

  def validate(csvFile: String, csvEncoding: Charset, validateCsvEncoding: Boolean, csvSchemaFile: String, csvSchemaEncoding: Charset, validateCsvSchemaEncoding: Boolean, failFast: Boolean, pathSubstitutionsList: JList[Substitution], enforceCaseSensitivePathChecks: Boolean, trace: Boolean, progress: ProgressCallback): JList[FailMessage] = {
    val sProgressCallback = new SProgressCallback {
      override def update(complete: this.type#Percentage) = progress.update(complete)
    }
    validate(csvFile, csvEncoding, validateCsvEncoding, csvSchemaFile, csvSchemaEncoding, validateCsvSchemaEncoding, failFast, pathSubstitutionsList, enforceCaseSensitivePathChecks, trace, Some(sProgressCallback))
  }

  private def validate(csvFile: String, csvEncoding: Charset, validateCsvEncoding: Boolean, csvSchemaFile: String, csvSchemaEncoding: Charset, validateCsvSchemaEncoding: Boolean, failFast: Boolean, pathSubstitutionsList: JList[Substitution], enforceCaseSensitivePathChecks: Boolean, trace: Boolean, progress: Option[SProgressCallback]): JList[FailMessage] = {

    import scala.jdk.CollectionConverters._

    val pathSubs: List[(String,String)] = pathSubstitutionsList.asScala.map( x => (x.getFrom, x.getTo)).toList

    val csvTextFile = TextFile(Paths.get(csvFile), csvEncoding, validateCsvEncoding)
    val csvSchemaTextFile = TextFile(Paths.get(csvSchemaFile), csvSchemaEncoding, validateCsvSchemaEncoding)

    checkFilesReadable(csvTextFile.file :: csvSchemaTextFile.file :: Nil) match {
      case Validated.Invalid(errors) =>
        errors.map{ asJavaMessage(_) }.toList.asJava

      case Validated.Valid(_) =>
        val validator = createValidator(failFast, pathSubs, enforceCaseSensitivePathChecks, trace)
        validator.parseSchema(csvSchemaTextFile) match {

          case Validated.Invalid(errors) =>
            errors.map(asJavaMessage(_)).toList.asJava

          case Validated.Valid(schema) =>
            validator.validate(csvTextFile, schema, progress) match {
              case Validated.Invalid(errors) => errors.map(asJavaMessage(_)).toList.asJava
              case Validated.Valid(_) => new JArrayList[FailMessage]
            }
        }
    }
  }

  def validate(csvData: JReader, csvSchema: JReader, failFast: Boolean, pathSubstitutionsList: JList[Substitution], enforceCaseSensitivePathChecks: Boolean, trace: Boolean): JList[FailMessage] =
    validate(csvData, csvSchema, failFast, pathSubstitutionsList, enforceCaseSensitivePathChecks, trace, None)

  def validate(csvData: JReader, csvSchema: JReader, failFast: Boolean, pathSubstitutionsList: JList[Substitution], enforceCaseSensitivePathChecks: Boolean, trace: Boolean, progress: ProgressCallback): JList[FailMessage] = {
    val sProgressCallback = new SProgressCallback {
      override def update(complete: this.type#Percentage) = progress.update(complete)
    }
    validate(csvData, csvSchema, failFast, pathSubstitutionsList, enforceCaseSensitivePathChecks, trace, Some(sProgressCallback))
  }

  private def validate(csvData: JReader, csvSchema: JReader, failFast: Boolean, pathSubstitutionsList: JList[Substitution],  enforceCaseSensitivePathChecks: Boolean, trace: Boolean, progress: Option[SProgressCallback]): JList[FailMessage] = {

    import scala.jdk.CollectionConverters._

    val pathSubs: List[(String,String)] = pathSubstitutionsList.asScala.map( x => (x.getFrom, x.getTo)).toList

    val validator = createValidator(failFast, pathSubs, enforceCaseSensitivePathChecks, trace)
    validator.parseSchema(csvSchema) match {

      case Validated.Invalid(errors) =>
        errors.map(asJavaMessage(_)).toList.asJava

      case Validated.Valid(schema) =>
        validator.validate(csvData, schema, progress) match {
          case Validated.Invalid(errors) => errors.map(asJavaMessage(_)).toList.asJava
          case Validated.Valid(_) => new JArrayList[FailMessage]
        }
    }
  }

  private def asJavaMessage(f: SFailMessage): FailMessage = f match {
    case SFailMessage(ValidationWarning, msg, lineNr, columnIdx) => new WarningMessage(msg, lineNr.getOrElse(-1), columnIdx.getOrElse(-1)).asInstanceOf[FailMessage]
    case SFailMessage(ValidationError, msg, lineNr, columnIdx) => new ErrorMessage(msg, lineNr.getOrElse(-1), columnIdx.getOrElse(-1)).asInstanceOf[FailMessage]
    case SFailMessage(SchemaDefinitionError, msg, lineNr, columnIdx) => new ErrorMessage(msg, lineNr.getOrElse(-1), columnIdx.getOrElse(-1)).asInstanceOf[FailMessage]
  }
}
