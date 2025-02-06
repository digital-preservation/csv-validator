/*
 * Copyright (c) 2013, The National Archives <digitalpreservation@nationalarchives.gov.uk>
 * https://www.nationalarchives.gov.uk
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package uk.gov.nationalarchives.csv.validator


import cats.data.{Chain, Validated, ValidatedNel}
import cats.syntax.all._
import com.univocity.parsers.common.TextParsingException
import com.univocity.parsers.csv.{CsvParser, CsvParserSettings}
import org.apache.commons.io.input.BOMInputStream
import uk.gov.nationalarchives.csv.validator.api.TextFile
import uk.gov.nationalarchives.csv.validator.metadata.{Cell, Row}
import uk.gov.nationalarchives.csv.validator.schema._
import uk.gov.nationalarchives.utf8.validator.{Utf8Validator, ValidationHandler}

import java.io.{BufferedInputStream, IOException, InputStreamReader => JInputStreamReader, LineNumberReader => JLineNumberReader, Reader => JReader}
import java.nio.charset.{Charset, StandardCharsets}
import java.nio.file.{Files, Path}
import scala.annotation.tailrec
import scala.language.{postfixOps, reflectiveCalls}
import scala.util.{Failure, Success, Try, Using}

//error reporting classes
sealed trait ErrorType
case object ValidationWarning extends ErrorType
case object ValidationError extends ErrorType
case object SchemaDefinitionError extends ErrorType
case class FailMessage(`type`: ErrorType, message : String, lineNumber: Option[Int] = None, columnIndex: Option[Int] = None) //TODO(AR) consider a better name, e.g. CsvValidationFailure
object FailMessage {
  def isWarning : PartialFunction[FailMessage, FailMessage] = {
    case fm @ FailMessage(ValidationWarning, _, _, _) => fm
  }

  def isError : PartialFunction[FailMessage, FailMessage] = {
    case fm @ FailMessage(ValidationError, _, _, _) => fm
  }

  def isSchemaDefinitionError : PartialFunction[FailMessage, FailMessage] = {
    case fm @ FailMessage(SchemaDefinitionError, _, _, _) => fm
  }
}



case class ProgressFor(rowsToValidate: Int, progress: ProgressCallback)

trait MetaDataValidator {
  // Helper functions for checking if a result contains a warning or error.
  def containsErrors(e: MetaDataValidation[Any]): Boolean = e.fold(_.collectFirst(FailMessage.isError).nonEmpty, _ => false)

  def containsWarnings(e: MetaDataValidation[Any]): Boolean = e.fold(_.collectFirst(FailMessage.isWarning).nonEmpty, _ => false)

  type MetaDataValidation[S] = ValidatedNel[FailMessage, S]

  @deprecated("use validateReader or validateCsvFile")
  def validate(
    csv: JReader,    
    schema: Schema,
    maxCharsPerCell: Int = 4096,
    progress: Option[ProgressCallback]
  ): MetaDataValidation[Any] = {
    var results: Chain[List[FailMessage]] = Chain.empty
    validateReader(
      csv,
      schema,
      maxCharsPerCell,
      progress,
      {
        case Validated.Invalid(x) => results = results :+ x.toList
        case _ =>
      }
    )
    results.toList.flatten.toNel match {
      case None => ().valid
      case Some(errors) => Validated.invalid(errors)
    }
  }

  def validateReader(
    csv: JReader,
    schema: Schema,
    maxCharsPerCell: Int,
    progress: Option[ProgressCallback],
    rowCallback: MetaDataValidation[Any] => Unit
  ): Boolean = {
    //try to find the number of rows for the
    //purposes pf reporting progress
    //can only do that if we can reset()
    //on the reader
    val pf = if(csv.markSupported()) {
      progress.map {
        p =>
          csv.mark(Integer.MAX_VALUE)
          val pf = ProgressFor(countRows(csv, schema), p)
          csv.reset()
          pf
      }
    } else {
      None
    }

    validateKnownRows(csv, schema, maxCharsPerCell, pf, rowCallback)
  }

  def createCsvParser(schema: Schema, maxCharsPerCell: Int): CsvParser = {
    val separator: Char = schema.globalDirectives.collectFirst {
      case Separator(sep) =>
        sep
    }.getOrElse(CSV_RFC1480_SEPARATOR)

    val quote: Option[Char] = schema.globalDirectives.collectFirst {
      case q: Quoted =>
        CSV_RFC1480_QUOTE_CHARACTER
    }

    val settings = new CsvParserSettings()
    val format = settings.getFormat
    format.setDelimiter(separator)
    quote.map(format.setQuote)

    /* Set RFC 1480 settings */
    settings.setIgnoreLeadingWhitespaces(false)
    settings.setIgnoreTrailingWhitespaces(false)
    settings.setLineSeparatorDetectionEnabled(true)
    settings.setMaxCharsPerColumn(maxCharsPerCell)
    // TODO(AR) should we be friendly and auto-detect line separator, or enforce RFC 1480?
    format.setQuoteEscape(CSV_RFC1480_QUOTE_ESCAPE_CHARACTER)
    //format.setLineSeparator(CSV_RFC1480_LINE_SEPARATOR)  // CRLF

    //we need a better CSV Reader!
    new CsvParser(settings)
  }


  def validateKnownRows(
    csv: JReader,
    schema: Schema,
    maxCharsPerCell: Int,
    progress: Option[ProgressFor],
    rowCallback: MetaDataValidation[Any] => Unit
  ): Boolean = {

    val parser = createCsvParser(schema, maxCharsPerCell)

    val result : Try[Boolean] = Using {
      parser.beginParsing(csv)
      parser
    } {
      reader =>

        // if 'no header' is set but the file is empty and 'permit empty' has not been set - this is an error
        // if 'no header' is not set and the file is empty - this is an error
        // if 'no header' is not set and 'permit empty' is not set but the file contains only one line - this is an error


        val rowIt = new RowIterator(reader, progress, maxCharsPerCell)

        val maybeNoData =
          if (schema.globalDirectives.contains(NoHeader())) {
            if (!rowIt.hasNext && !schema.globalDirectives.contains(PermitEmpty())) {
              Some(FailMessage(ValidationError, "metadata file is empty but this has not been permitted").invalidNel[Any])
            } else {
              None
            }
          } else {
            if(!rowIt.hasNext) {
              Some(FailMessage(ValidationError, "metadata file is empty but should contain at least a header").invalidNel[Any])
            } else {
              val header = rowIt.skipHeader()
              val headerValidation = validateHeader(header, schema)
              headerValidation.orElse {
                if(!rowIt.hasNext && !schema.globalDirectives.contains(PermitEmpty())) {
                  Some(FailMessage(ValidationError, "metadata file has a header but no data and this has not been permitted").invalidNel[Any])
                } else {
                  None
                }
              }
            }
          }

        maybeNoData match {
          case Some(noData) =>
            rowCallback(noData)
            false
          case None =>
            validateRows(rowIt, schema, rowCallback)
        }

    } (_.stopParsing());

    result match {
      case util.Success(metadataValidation) =>
        metadataValidation
      case util.Failure(ts) =>
        //TODO(AR) emit all errors not just first!
        rowCallback(FailMessage(ValidationError, ts.toString).invalidNel[Any])
        false
//      ts.toList.map(t => FailMessage(ValidationError, t.toString).failureNel[Any]).sequence[MetaDataValidation, Any]
    }
  }

  /**
    * Return the column at the index columnIndex
    * @param rows the row iterator
    * @param columnIndex the index of the column
    * @return List of string of all element at the columnIndex
    */
  def getColumn(rows: Iterator[Row], columnIndex: Int): List[String] =
    rows.foldLeft(List[String]()){ (acc,row) =>
      acc :+ filename(row, columnIndex)
    }

  def filename(row: Row,titleIndex: Int): String = row.cells(titleIndex).value

  def validateRows(
    rows: Iterator[Row],
    schema: Schema,
    rowCallback: MetaDataValidation[Any] => Unit
  ): Boolean

  def validateHeader(header: Row, schema: Schema): Option[MetaDataValidation[Any]] = {
    val icnc: Option[IgnoreColumnNameCase] = schema.globalDirectives.collectFirst {case i @ IgnoreColumnNameCase() => i }

    def toggleCase(s: String): String =
      if (icnc.isDefined) s.toLowerCase else s


    val headerList = header.cells.map(header => toggleCase(header.value))
    val schemaHeader = schema.columnDefinitions.map(header => toggleCase(header.id.value))

    if (headerList.sameElements(schemaHeader))
      None
    else
      Some(FailMessage(ValidationError, s"Metadata header, cannot find the column headers - ${Util.diff(schemaHeader.toSet, headerList.toSet).mkString(", ")} - .${if (icnc.isEmpty) "  (Case sensitive)" else ""}").invalidNel[Any])
  }

  def validateRow(row: Row,  schema: Schema, mayBeLast: Option[Boolean] = None): MetaDataValidation[Any] = {
    val totalColumnsV = totalColumns(row, schema)
    val rulesV = rules(row,  schema, mayBeLast)
    (totalColumnsV, rulesV).mapN { _ :: _ }
  }

  def validateUtf8Encoding(file: Path): MetaDataValidation[Any] = {

    val validationHandler = new ValidationHandler {
      var errors: List[(Long, String)] = List()

      override def error(message: String, byteOffset: Long): Unit = {
        errors ::= (byteOffset, message)
      }
    }

    new Utf8Validator(validationHandler).validate(file.toFile)

    validationHandler.errors.toNel match {
      case None => true.validNel
      case Some(nel) => {
        val ret = nel.reverse.map {
          case (offset, message) => FailMessage(ValidationError, s"[UTF-8 Error][@$offset] ${message}")
        }
        ret.invalid
      }
    }
  }

  private def totalColumns(row: Row, schema: Schema): MetaDataValidation[Any] = {
    val tc: Option[TotalColumns] = schema.globalDirectives.collectFirst {
      case t@TotalColumns(_) => t
    }

    if (tc.isEmpty || tc.get.numberOfColumns == row.cells.length) true.validNel[FailMessage]
    else FailMessage(ValidationError, s"Expected @totalColumns of ${tc.get.numberOfColumns} and found ${row.cells.length} on line ${row.lineNumber}", Some(row.lineNumber), Some(row.cells.length)).invalidNel[Any]
  }

  protected def rules(row: Row, schema: Schema, mayBeLast: Option[Boolean] = None): MetaDataValidation[List[Any]]

  protected def validateCell(columnIndex: Int, cells: (Int) => Option[Cell], row: Row, schema: Schema, mayBeLast: Option[Boolean] = None): MetaDataValidation[Any] = {
    cells(columnIndex) match {
      case Some(c) => rulesForCell(columnIndex, row, schema, mayBeLast)
      case _ => FailMessage(ValidationError, s"Missing value at line: ${row.lineNumber}, column: ${schema.columnDefinitions(columnIndex).id}", Some(row.lineNumber), Some(columnIndex)).invalidNel[Any]
    }
  }

  protected def toWarnings(results: Rule#RuleValidation[Any], lineNumber: Int, columnIndex: Int): MetaDataValidation[Any] = results.leftMap(_.map(FailMessage(ValidationWarning, _, Some(lineNumber), Some(columnIndex))))
  protected def toErrors(results: Rule#RuleValidation[Any], lineNumber: Int, columnIndex: Int): MetaDataValidation[Any] = results.leftMap(_.map(FailMessage(ValidationError, _, Some(lineNumber), Some(columnIndex))))

  protected def rulesForCell(columnIndex: Int, row: Row, schema: Schema, mayBeLast: Option[Boolean] = None): MetaDataValidation[Any]

  protected def countRows(textFile: TextFile, schema: Schema): Int = {
    withReader(textFile) {
      reader =>
        countRows(reader, schema)
    }
  }

  protected def countRows(reader: JReader, schema: Schema): Int = {
    val rowsAsHeader =  if(schema.globalDirectives.contains(NoHeader())) 0 else 1
    Try {
      val lineReader = new JLineNumberReader(reader) // don't close this JLineNumberReader, because it automatically close original reader.
                                                     // It's resource/memory safe. JLineReader will be colected by GS.
      @tailrec
      def readAll(): Int = {
        val result = Option(lineReader.readLine())
        if(result.isEmpty) {
          lineReader.getLineNumber() + 1 //start from 1 not 0
        } else {
          readAll()
        }
      }
      readAll()
    }.map(_ - rowsAsHeader) getOrElse -1
  }

  protected def withReader[B](textFile: TextFile)(fn: JReader => B): B = {
    def inputStreamReader(encoding: Charset) : JInputStreamReader = {
      val bis = new BufferedInputStream(Files.newInputStream(textFile.file))
      val is = if(encoding == StandardCharsets.UTF_8) {
        new BOMInputStream(bis)
      } else {
        bis
      }
      new JInputStreamReader(is, encoding)
    }

    Using(inputStreamReader(textFile.encoding))(fn) match {
      case util.Failure(ioError) =>
        throw ioError
      case util.Success(result) =>
        result
    }
  }
}

trait ProgressCallback {

  /**
    * A percentage is always between
    * 0 and 100 inclusive
    */
  type Percentage = Float


  def update(complete: Percentage): Unit

  def update(total: Int, processed: Int): Unit = update((processed.toFloat / total.toFloat) * 100)
}

class RowIterator(parser: CsvParser, progress: Option[ProgressFor], maxCharsPerCell: Int) extends Iterator[Row] {

  private var index = 1
  private var current = toRow(Try(parser.parseNext()))
  private var potentialHeaderRow: Option[Row] = None

  @throws(classOf[IOException])
  override def next(): Row = {
    val row = current match {
      case Success(row) =>
        if(index == 1 && potentialHeaderRow.isEmpty) potentialHeaderRow = Some(row) // this is here in case the old API is used that doesn't call 'skipHeader'
        row
      case Failure(ex: TextParsingException) if(ex.toString.contains("exceeds the maximum number of characters")) =>
        val cellLocationMsg =
          potentialHeaderRow match {
            case Some(headerRow) => s"in the cell located at line: ${ex.getLineIndex}, column: ${headerRow.cells(ex.getColumnIndex).value},"
            case None => s"in column ${ex.getColumnIndex + 1} of the header row"
          }

        val customMessage =
          s"The number of characters $cellLocationMsg is larger than the maximum number of characters allowed in a cell ($maxCharsPerCell); increase this limit and re-run."
        throw new Exception(customMessage)
      case Failure(ex) => throw ex
    }

    //move to the next
    this.index = index + 1
    this.current = toRow(Try(parser.parseNext()))

    progress map {
      p =>
        if(p.rowsToValidate != -1) {
          p.progress.update(p.rowsToValidate, index)
        }
    }

    row
  }

  @throws(classOf[IOException])
  def skipHeader(): Row = {
    this.index = index - 1
    val row = next()
    this.potentialHeaderRow = Some(row)
    row
  }

  override def hasNext: Boolean = current match {
    case Failure(ex: NullPointerException) => false
    case _ => true
  }

  private def toRow(rowData: Try[Array[String]]): Try[Row] = rowData.map(data => Row(data.toList.map(d => Cell(Option(d).getOrElse(""))), index))
}
