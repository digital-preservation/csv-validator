/*
 * Copyright (c) 2013, The National Archives digitalpreservation@nationalarchives.gov.uk
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package uk.gov.tna.dri.schema

import scalaz.Scalaz._
import scala.util.parsing.input.Positional
import uk.gov.tna.dri.metadata.Row

abstract class Rule(name: String, val argProviders: ArgProvider*) extends Positional {

  type RuleValidation[A] = ValidationNEL[String, A]

  var explicitColumn: Option[String] = None

  def evaluate(columnIndex: Int, row: Row, schema: Schema): RuleValidation[Any] = {
    if (valid(cellValue(columnIndex, row, schema), schema.columnDefinitions(columnIndex), columnIndex, row, schema)) true.successNel[String] else fail(columnIndex, row, schema)
  }

  def valid(cellValue: String, columnDefinition: ColumnDefinition, columnIndex: Int, row: Row, schema: Schema): Boolean

  def fail(columnIndex: Int, row: Row, schema: Schema): RuleValidation[Any] = {
    val columnDefinition = schema.columnDefinitions(columnIndex)
    s"$toError fails for line: ${row.lineNumber}, column: ${columnDefinition.id}, ${toValueError(row,columnIndex)}".failNel[Any]
  }

  def cellValue(columnIndex: Int, row: Row, schema: Schema) = explicitColumn match {
    case Some(columnName) => row.cells(columnNameToIndex(schema, columnName)).value
    case None => row.cells(columnIndex).value
  }

  def explicitName = explicitColumn match {
    case Some(colName) => "$" + colName + "/"
    case None => ""
  }

  def ruleName = explicitName + name

  def columnNameToIndex(schema: Schema, name: String): Int = {
    try {
      schema.columnDefinitions.zipWithIndex.filter{ case (c,i) => c.id == name}.head._2
    } catch {
      // this should be fixed in the schema validator, preventing this from ever happening
      case _: java.util.NoSuchElementException => println( s"Error:   Unable to find: $name for line: ${pos.line}, column: ${pos.column}"); 0
      case _: Throwable => println( s"Error: with: $name"); 0
    }
  }

  def toValueError(row: Row, columnIndex:Int ) =  s"""value: ${'"'}${row.cells(columnIndex).value}${'"'}"""

  def toError = s"""$ruleName""" + (if (argProviders.isEmpty) "" else "(" + argProviders.foldLeft("")((a, b) => (if (a.isEmpty) "" else a + ", ") + b.toError) + ")")
}

