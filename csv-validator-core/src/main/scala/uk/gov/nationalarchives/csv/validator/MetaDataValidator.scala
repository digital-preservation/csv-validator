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
import uk.gov.nationalarchives.csv.validator.schema._
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

        //if there is header and no metadata, file must skip the first row and return true
        //if there is no header and metadata, file must have at least one row
        //if there is header and metadata, file must skip first the firs row and find metadata

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