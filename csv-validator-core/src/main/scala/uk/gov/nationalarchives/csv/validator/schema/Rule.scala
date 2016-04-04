/**
 * Copyright (c) 2013, The National Archives <digitalpreservation@nationalarchives.gov.uk>
 * http://www.nationalarchives.gov.uk
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package uk.gov.nationalarchives.csv.validator.schema

import org.joda.time.DateTime
import uk.gov.nationalarchives.csv.validator.metadata.Row

import scala.collection.mutable.MutableList
import scala.util.Try
import scala.util.parsing.input.Positional
import scalaz._
import scalaz.Scalaz._

abstract class Rule(name: String, val argProviders: ArgProvider*) extends Positional {

  type RuleValidation[A] = ValidationNel[String, A]

  var explicitColumn: Option[ColumnReference] = None

  def findColumnReference(): Option[ColumnReference] = {
    if (explicitColumns.nonEmpty){
      val index =  explicitColumnIndex
      val result = explicitColumns.get(index)
      if (index + 1 == explicitColumns.length)
        explicitColumnIndex = 0
      else
        explicitColumnIndex = index + 1
      result
    }
    else{
      explicitColumn = None
      None
    }
  }


  def findColumnRefence(rule: Rule): Option[ColumnReference] =
    rule.findColumnReference()

  var explicitColumnIndex = 0

  val explicitColumns: MutableList[ColumnReference] = MutableList()

  def evaluate(columnIndex: Int, row: Row,  schema: Schema, mayBeLast: Option[Boolean] = None): RuleValidation[Any] = {
    if (valid(cellValue(columnIndex, row, schema), schema.columnDefinitions(columnIndex), columnIndex, row, schema, mayBeLast))
      true.successNel[String]
    else fail(columnIndex, row, schema)
  }


  def valid(cellValue: String, columnDefinition: ColumnDefinition, columnIndex: Int,
            row: Row, schema: Schema, mayBeLast: Option[Boolean] = None): Boolean =
    evaluate(columnIndex, row, schema).isSuccess


  def fail(columnIndex: Int, row: Row, schema: Schema): RuleValidation[Any] = {
    val columnDefinition = schema.columnDefinitions(columnIndex)
    s"$toError fails for line: ${row.lineNumber}, column: ${columnDefinition.id}, ${toValueError(row,columnIndex)}".failureNel[Any]
  }

  def cellValue(columnIndex: Int, row: Row, schema: Schema): String = {
    explicitColumn match {
      case Some(columnRef) =>
        columnRef.referenceValueEx(columnIndex, row, schema)
      case None =>
        row.cells(columnIndex).value
    }
  }

  def explicitName: Option[String] = explicitColumn.map("$" + _.ref + "/")


  def ruleName: String = explicitName.getOrElse("") + name

  def columnIdentifierToIndex(schema: Schema, id: ColumnIdentifier): Int = {
    try {
      schema.columnDefinitions.zipWithIndex.filter{ case (c,i) => c.id == id}.head._2
    } catch {
      // TODO this should be fixed in the uk.gov.nationalarchives.csv.validator.schema validator, preventing this from ever happening
      case _: java.util.NoSuchElementException => println( s"Error:   Unable to find: $id for line: ${pos.line}, column: ${pos.column}"); 0
      case _: Throwable => println( s"Error: with: $id"); 0
    }
  }



  def toValueError(row: Row, columnIndex:Int ) =
    s"""value: ${'"'}${row.cells(columnIndex).value}${'"'}"""


  def toError =
    s"""$ruleName""" + (if (argProviders.isEmpty) "" else "(" + argProviders.foldLeft("")((a, b) => (if (a.isEmpty) "" else a + ", ") + b.toError) + ")")

}

abstract class PatternRule(name: String, pattern: String) extends Rule(name) {
  override def valid(cellValue: String, columnDefinition: ColumnDefinition, columnIndex: Int, row: Row, schema: Schema, mayBeLast: Option[Boolean] = None): Boolean = cellValue matches pattern
}

trait DateParser {
  def parse(dateStr: String): Try[DateTime]
}

abstract class DateRule(name: String, dateRegex: String, dateParser: DateParser) extends PatternRule(name, dateRegex) {
  override def valid(cellValue: String, columnDefinition: ColumnDefinition, columnIndex: Int, row: Row, schema: Schema, mayBeLast: Option[Boolean] = None): Boolean = {
    super.valid(cellValue, columnDefinition, columnIndex, row, schema) match {
      case true => dateParser.parse(cellValue).isSuccess
      case _ => false
    }
  }
}