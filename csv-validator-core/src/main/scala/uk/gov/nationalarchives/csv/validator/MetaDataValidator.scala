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
import java.io.{IOException, Reader}
import resource._
import uk.gov.nationalarchives.csv.validator.schema.{NoHeader, Schema}
import uk.gov.nationalarchives.csv.validator.metadata.{Row, Cell}
import scala.collection.JavaConversions._

import au.com.bytecode.opencsv.{CSVParser, CSVReader}
import uk.gov.nationalarchives.csv.validator.metadata.Row

sealed abstract class FailMessage(val msg:String)
case class WarningMessage(message:String) extends FailMessage(message)
case class ErrorMessage(message:String) extends FailMessage(message)
case class SchemaMessage(message:String) extends FailMessage(message)

trait MetaDataValidator {
  type MetaDataValidation[S] = ValidationNel[FailMessage, S]

  def validate(csv: Reader, schema: Schema): MetaDataValidation[Any] = {

    //TODO CSVReader does not appear to be RFC 4180 compliant as it does not support escaping a double-quote with a double-quote between double-quotes
    //we need a better CSV Reader!
    //val rows = new CSVReader(csv).readAll().toList



    managed(new CSVReader(csv, CSVParser.DEFAULT_SEPARATOR, CSVParser.DEFAULT_QUOTE_CHARACTER, CSVParser.NULL_CHARACTER)) map {
      reader =>

        val rowIt = new RowIterator(reader)

        if(!rowIt.hasNext) {
          ErrorMessage("metadata file is empty").failNel[Any]
        } else {
          //if the csv has a header, skip over it
          if(!schema.globalDirectives.contains(NoHeader())) {
            val header = rowIt.skipHeader()

          }

          if(!rowIt.hasNext) {
            ErrorMessage("metadata file has a header but no data").failNel[Any]
          } else {
            validateRows(rowIt, schema)
          }
        }
    } either match {
      case Right(metadataValidation) =>
        metadataValidation

      case Left(ts) =>
        for(t <- ts) { //TODO remove
          t.printStackTrace()
        }

        //TODO emit all errors not just first
        ErrorMessage(ts(0).toString).failNel[Any]
        //ts.toList.map(t => ErrorMessage(t.toString).failNel[Any]).sequence[MetaDataValidation, Any]
    }
  }

  def validateRows(rows: Iterator[Row], schema: Schema): MetaDataValidation[Any]
}

class RowIterator(reader: CSVReader) extends Iterator[Row] {

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