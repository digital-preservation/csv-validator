/**
 * Copyright (c) 2013, The National Archives <digitalpreservation@nationalarchives.gov.uk>
 * http://www.nationalarchives.gov.uk
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package uk.gov.nationalarchives.csv.validator

import scalaz._, Scalaz._
import java.io.{IOException, Reader => JReader, InputStreamReader => JInputStreamReader, FileInputStream => JFileInputStream, LineNumberReader => JLineNumberReader}
import resource._
import uk.gov.nationalarchives.csv.validator.schema.{Optional => SchemaOptional, _}
import uk.gov.nationalarchives.csv.validator.metadata.Cell

import au.com.bytecode.opencsv.{CSVParser, CSVReader}
import uk.gov.nationalarchives.csv.validator.metadata.Row
import scala.annotation.tailrec
import uk.gov.nationalarchives.csv.validator.api.TextFile

sealed abstract class FailMessage(val msg:String)
case class WarningMessage(message:String) extends FailMessage(message)
case class ErrorMessage(message:String) extends FailMessage(message)
case class SchemaMessage(message:String) extends FailMessage(message)

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
              val header = rowIt.skipHeader()
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
    
    //TODO not used
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
              Some(ErrorMessage("metadata file is empty but this has not been permitted").failNel[Any])
            } else {
              None
            }
          } else {
            if(!rowIt.hasNext) {
              Some(ErrorMessage("metadata file is empty but should contain at least a header").failNel[Any])
            } else {
              val header = rowIt.skipHeader()
              if(!rowIt.hasNext && !schema.globalDirectives.contains(PermitEmpty())) {
                Some(ErrorMessage("metadata file has a header but no data and this has not been permitted").failNel[Any])
              } else {
                None
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
        ErrorMessage(ts(0).toString).failNel[Any]
        //ts.toList.map(t => ErrorMessage(t.toString).failNel[Any]).sequence[MetaDataValidation, Any]
    }
  }

  def validateRows(rows: Iterator[Row], schema: Schema): MetaDataValidation[Any]

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

  protected def validateRow(row: Row, schema: Schema): MetaDataValidation[Any] = {
    val totalColumnsV = totalColumns(row, schema)
    val rulesV = rules(row, schema)
    (totalColumnsV |@| rulesV) { _ :: _ }
  }

  protected def totalColumns(row: Row, schema: Schema): MetaDataValidation[Any] = {
    val tc: Option[TotalColumns] = schema.globalDirectives.collectFirst {
      case t@TotalColumns(_) => t
    }

    if (tc.isEmpty || tc.get.numberOfColumns == row.cells.length) true.successNel[FailMessage]
    else ErrorMessage(s"Expected @totalColumns of ${tc.get.numberOfColumns} and found ${row.cells.length} on line ${row.lineNumber}").failNel[Any]
  }

  private def rules(row: Row, schema: Schema): MetaDataValidation[List[Any]] = {
    val cells: (Int) => Option[Cell] = row.cells.lift
    val v = for {(columnDefinition, columnIndex) <- schema.columnDefinitions.zipWithIndex} yield validateCell(columnIndex, cells, row, schema)
    v.sequence[MetaDataValidation, Any]
  }

  protected def validateCell(columnIndex: Int, cells: (Int) => Option[Cell], row: Row, schema: Schema): MetaDataValidation[Any] = {
    cells(columnIndex) match {
      case Some(c) => rulesForCell(columnIndex, row, schema)
      case _ => ErrorMessage(s"Missing value at line: ${row.lineNumber}, column: ${schema.columnDefinitions(columnIndex).id}").failNel[Any]
    }
  }

  protected def rulesForCell(columnIndex: Int, row: Row, schema: Schema): MetaDataValidation[Any] = {

    val columnDefinition = schema.columnDefinitions(columnIndex)

    def isWarningDirective: Boolean = columnDefinition.directives.contains(Warning())
    def isOptionDirective: Boolean = columnDefinition.directives.contains(SchemaOptional())

    def convert2Warnings(results:Rule#RuleValidation[Any]): MetaDataValidation[Any] = {
      results.leftMap(_.map(WarningMessage))
    }

    def convert2Errors(results:Rule#RuleValidation[Any]): MetaDataValidation[Any] = {
      results.leftMap(_.map(ErrorMessage))
    }

    if (row.cells(columnIndex).value.trim.isEmpty && isOptionDirective) true.successNel
    else columnDefinition.rules.map(_.evaluate(columnIndex, row, schema)).map{ ruleResult:Rule#RuleValidation[Any] => {
      if(isWarningDirective) convert2Warnings(ruleResult) else convert2Errors(ruleResult)
    }}.sequence[MetaDataValidation, Any]
  }

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
  def skipHeader() = {
    this.index = index - 1
    next()
  }

  override def hasNext: Boolean = current.nonEmpty

  private def toRow(rowData: Option[Array[String]]): Option[Row] = rowData.map(data => Row(data.toList.map(Cell(_)), index))
}