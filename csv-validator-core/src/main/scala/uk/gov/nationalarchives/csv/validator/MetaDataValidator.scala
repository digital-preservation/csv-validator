/**
 * Copyright (c) 2013, The National Archives <digitalpreservation@nationalarchives.gov.uk>
 * http://www.nationalarchives.gov.uk
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package uk.gov.nationalarchives.csv.validator


import scala.language.postfixOps
import scalaz._, Scalaz._
import java.io.{IOException, Reader => JReader, InputStreamReader => JInputStreamReader, FileInputStream => JFileInputStream, LineNumberReader => JLineNumberReader}
import resource._
import uk.gov.nationalarchives.csv.validator.schema._
import uk.gov.nationalarchives.csv.validator.metadata.Cell

import com.opencsv.{CSVParser, CSVReader}
import uk.gov.nationalarchives.csv.validator.metadata.Row
import scala.annotation.tailrec
import uk.gov.nationalarchives.csv.validator.api.TextFile

sealed abstract class FailMessage(val msg:String, val lineNr:Option[Int], val colIdx:Option[Int])
case class WarningMessage(message:String, lineNumber: Option[Int] = None, columnIndex: Option[Int] = None) extends FailMessage(message, lineNumber, columnIndex)
case class ErrorMessage(message:String, lineNumber: Option[Int] = None, columnIndex: Option[Int] = None) extends FailMessage(message, lineNumber, columnIndex)
case class SchemaMessage(message:String, lineNumber: Option[Int] = None, columnIndex: Option[Int] = None) extends FailMessage(message, lineNumber, columnIndex)

case class ProgressFor(rowsToValidate: Int, progress: ProgressCallback)

trait MetaDataValidator {
  type MetaDataValidation[S] = ValidationNel[FailMessage, S]

  def validate(csv: JReader, schema: Schema, progress: Option[ProgressCallback]): MetaDataValidation[Any] = {

    //try to find the number of rows for the
    //purposes pf reporting progress
    //can only do that if we can reset()
    //on the reader
    val pf = if(csv.markSupported()) {
      progress.map {
        p =>
          csv.mark(Integer.MAX_VALUE)
          val pf = ProgressFor(countRows(csv), p)
          csv.reset()
          pf
      }
    } else {
      None
    }

    validateKnownRows(csv, schema, pf)
  }

  /**
   * Browse csv File and return all the titleIndex as a list
   * @param csv the CSV reader
   * @param schema the Schema
   * @param columnIndex the index of the column to be return
   * @return all the element of the column columnIndex
   */
  def getColumn(csv: JReader, schema: Schema, columnIndex: Int): List[String] = {

    val separator = schema.globalDirectives.collectFirst {
      case Separator(sep) =>
        sep
    }.getOrElse(CSVParser.DEFAULT_SEPARATOR)

    val quote = schema.globalDirectives.collectFirst {
      case q: Quoted =>
        CSVParser.DEFAULT_QUOTE_CHARACTER
    }

    //TODO CSVReader does not appear to be RFC 4180 compliant as it does not support escaping a double-quote with a double-quote between double-quotes
    //TODO CSVReader does not seem to allow you to enable/disable quoted columns
    //we need a better CSV Reader!
    (managed(new CSVReader(csv, separator, CSVParser.DEFAULT_QUOTE_CHARACTER, CSVParser.NULL_CHARACTER)) map {
      reader =>
        // if 'no header' is set but the file is empty and 'permit empty' has not been set - this is an error
        // if 'no header' is not set and the file is empty - this is an error
        // if 'no header' is not set and 'permit empty' is not set but the file contains only one line - this is an error

        val rowIt = new RowIterator(reader, None)

        val maybeNoData =
          if (schema.globalDirectives.contains(NoHeader())) {
            if (!rowIt.hasNext && !schema.globalDirectives.contains(PermitEmpty())) {
              Some(ErrorMessage("metadata file is empty but this has not been permitted").failNel[Any])
            } else {
              None
            }
          } else {
            if(!rowIt.hasNext) {
              Some(ErrorMessage("metadata file is empty but should contain at least a header").failNel[Any])
            } else {
              if(!rowIt.hasNext && !schema.globalDirectives.contains(PermitEmpty())) {
                Some(ErrorMessage("metadata file has a header but no data and this has not been permitted").failNel[Any])
              } else {
                None
              }
            }
          }

        maybeNoData match {
          case Some(noData) =>
            Nil
          case None =>
            getColumn(rowIt, columnIndex)

        }
    } opt).getOrElse(Nil)
  }


  def validateKnownRows(csv: JReader, schema: Schema, progress: Option[ProgressFor]): MetaDataValidation[Any] = {

    val separator = schema.globalDirectives.collectFirst {
      case Separator(sep) =>
        sep
    }.getOrElse(CSVParser.DEFAULT_SEPARATOR)

    val quote = schema.globalDirectives.collectFirst {
      case q: Quoted =>
        CSVParser.DEFAULT_QUOTE_CHARACTER
    }

    //TODO CSVReader does not appear to be RFC 4180 compliant as it does not support escaping a double-quote with a double-quote between double-quotes
    //TODO CSVReader does not seem to allow you to enable/disable quoted columns
    //we need a better CSV Reader!
    managed(new CSVReader(csv, separator, CSVParser.DEFAULT_QUOTE_CHARACTER, CSVParser.NULL_CHARACTER)) map {
      reader =>

        // if 'no header' is set but the file is empty and 'permit empty' has not been set - this is an error
        // if 'no header' is not set and the file is empty - this is an error
        // if 'no header' is not set and 'permit empty' is not set but the file contains only one line - this is an error

        val rowIt = new RowIterator(reader, progress)

        val maybeNoData =
          if (schema.globalDirectives.contains(NoHeader())) {
            if (!rowIt.hasNext && !schema.globalDirectives.contains(PermitEmpty())) {
              Some(ErrorMessage("metadata file is empty but this has not been permitted").failureNel[Any])
            } else {
              None
            }
          } else {
            if(!rowIt.hasNext) {
              Some(ErrorMessage("metadata file is empty but should contain at least a header").failureNel[Any])
            } else {
              val header = rowIt.skipHeader()
              val headerValidation = validateHeader(header, schema)
              headerValidation.orElse{
                if(!rowIt.hasNext && !schema.globalDirectives.contains(PermitEmpty())) {
                  Some(ErrorMessage("metadata file has a header but no data and this has not been permitted").failureNel[Any])
                } else {
                  None
                }
              }
            }
          }

        maybeNoData match {
          case Some(noData) =>
            noData
          case None =>
            validateRows(rowIt, schema)
        }

    } either match {
      case Right(metadataValidation) =>
        metadataValidation

      case Left(ts) =>
        //TODO emit all errors not just first!
        ErrorMessage(ts(0).toString).failureNel[Any]
        //ts.toList.map(t => ErrorMessage(t.toString).failureNel[Any]).sequence[MetaDataValidation, Any]
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

  def filename(row: Row,titleIndex: Int): String = row.cells.map(_.value).apply(titleIndex)


  def validateRows(rows: Iterator[Row], schema: Schema): MetaDataValidation[Any]

  def validateHeader(header: Row, schema: Schema): Option[MetaDataValidation[Any]] = {
    val icnc: Option[IgnoreColumnNameCase] = schema.globalDirectives.collectFirst {case i @ IgnoreColumnNameCase() => i }

    def toggleCase(s: String): String =
      if (icnc.isDefined) s.toLowerCase else s


    val headerList = header.cells.map(header => toggleCase(header.value))
    val schemaHeader = schema.columnDefinitions.map(header => toggleCase(header.id.value))

    if (headerList.sameElements(schemaHeader))
      None
    else
      Some(ErrorMessage(s"Metadata header, cannot find the column headers - ${Util.diff(schemaHeader.toSet, headerList.toSet).mkString(", ")} - .${if (icnc.isEmpty) "  (Case sensitive)" else ""}").failNel[Any])
  }

  def validateRow(row: Row, schema: Schema): MetaDataValidation[Any] = {
    val totalColumnsV = totalColumns(row, schema)
    val rulesV = rules(row, schema)
    (totalColumnsV |@| rulesV) { _ :: _ }
  }

  private def totalColumns(row: Row, schema: Schema): MetaDataValidation[Any] = {
    val tc: Option[TotalColumns] = schema.globalDirectives.collectFirst {
      case t@TotalColumns(_) => t
    }

    if (tc.isEmpty || tc.get.numberOfColumns == row.cells.length) true.successNel[FailMessage]
    else ErrorMessage(s"Expected @totalColumns of ${tc.get.numberOfColumns} and found ${row.cells.length} on line ${row.lineNumber}", Some(row.lineNumber), Some(row.cells.length)).failureNel[Any]
  }

  protected def rules(row: Row, schema: Schema): MetaDataValidation[List[Any]]

  protected def validateCell(columnIndex: Int, cells: (Int) => Option[Cell], row: Row, schema: Schema): MetaDataValidation[Any] = {
    cells(columnIndex) match {
      case Some(c) => rulesForCell(columnIndex, row, schema)
      case _ => ErrorMessage(s"Missing value at line: ${row.lineNumber}, column: ${schema.columnDefinitions(columnIndex).id}", Some(row.lineNumber), Some(columnIndex)).failureNel[Any]
    }
  }

  protected def toWarnings(results: Rule#RuleValidation[Any], lineNumber: Int, columnIndex: Int): MetaDataValidation[Any] = results.leftMap(_.map(WarningMessage(_, Some(lineNumber), Some(columnIndex))))
  protected def toErrors(results: Rule#RuleValidation[Any], lineNumber: Int, columnIndex: Int): MetaDataValidation[Any] = results.leftMap(_.map(ErrorMessage(_, Some(lineNumber), Some(columnIndex))))

  protected def rulesForCell(columnIndex: Int, row: Row, schema: Schema): MetaDataValidation[Any]

  protected def countRows(textFile: TextFile): Int = {
    withReader(textFile) {
      reader =>
        countRows(reader)
    }
  }

  protected def countRows(reader: JReader): Int = {
    (managed(new JLineNumberReader(reader)) map {
      lineReader =>

        @tailrec
        def readAll(): Int = {
          val result = Option(lineReader.readLine())
          if(result.empty) {
            lineReader.getLineNumber() + 1 //start from 1 not 0
          } else {
            readAll()
          }
        }
        readAll()
    } opt) getOrElse -1
  }

  protected def withReader[B](textFile: TextFile)(fn: JReader => B): B = {
    managed(new JInputStreamReader(new JFileInputStream(textFile.file.path), textFile.encoding)).map {
      reader =>
        fn(reader)
    }.either match {
      case Left(ioError) =>
        throw ioError(0)
      case Right(result) =>
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


  def update(complete: Percentage)
}

class RowIterator(reader: CSVReader, progress: Option[ProgressFor]) extends Iterator[Row] {

  private var index = 1
  private var current = toRow(Option(reader.readNext()))

  @throws(classOf[IOException])
  override def next(): Row = {
    val row = current match {
      case Some(row) =>
        row
      case None => {
        throw new IOException("End of file")
      }
    }

    //move to the next
    this.index = index + 1
    this.current = toRow(Option(reader.readNext()))

    progress map {
      p =>
        if(p.rowsToValidate != -1) {
          p.progress.update((index.toFloat / p.rowsToValidate.toFloat) * 100)
        }
    }

    row
  }

  @throws(classOf[IOException])
  def skipHeader(): Row = {
    this.index = index - 1
    next()
  }

  override def hasNext: Boolean = current.nonEmpty

  private def toRow(rowData: Option[Array[String]]): Option[Row] = rowData.map(data => Row(data.toList.map(Cell(_)), index))
}
