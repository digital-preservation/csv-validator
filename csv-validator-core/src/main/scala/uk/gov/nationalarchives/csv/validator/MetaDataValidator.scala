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
import java.io.Reader
import uk.gov.nationalarchives.csv.validator.schema.{NoHeader, Schema}
import uk.gov.nationalarchives.csv.validator.metadata.{Cell, Row}
import scala.collection.JavaConversions._

import au.com.bytecode.opencsv.{CSVParser, CSVReader}

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
    val rows = new CSVReader(csv, CSVParser.DEFAULT_SEPARATOR, CSVParser.DEFAULT_QUOTE_CHARACTER, CSVParser.NULL_CHARACTER).readAll().toList  //TODO temp using NULL_CHARACTER as escape to work around non-RFC4180 compliance
    if (rows.isEmpty) ErrorMessage("metadata file is empty").failNel[Any] else {
      val rowsWithNoHeader = if (schema.globalDirectives.contains(NoHeader())) rows else rows.drop(1)
      if (rowsWithNoHeader.isEmpty) ErrorMessage("metadata file has a header but no data").failNel[Any]
      else validateRows(rowsWithNoHeader.map(_.toList).zipWithIndex.map(r => Row(r._1.map(Cell(_)), r._2 + 1)), schema)
    }
  }

  def validateRows(rows: List[Row], schema: Schema): MetaDataValidation[Any]
}