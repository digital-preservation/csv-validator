/**
 * Copyright (c) 2013, The National Archives <digitalpreservation@nationalarchives.gov.uk>
 * https://www.nationalarchives.gov.uk
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package uk.gov.nationalarchives.csv.validator.schema

import org.apache.commons.io.FilenameUtils
import uk.gov.nationalarchives.csv.validator.metadata.Row
import util.parsing.input.Positional

case class Schema(globalDirectives: List[GlobalDirective], columnDefinitions: List[ColumnDefinition], version: String = Schema.version)

object Schema {
  val version = "1.2"
}

abstract class GlobalDirective(val name: String) extends Positional

case class Separator(separatorChar: Char) extends GlobalDirective("separator")

case class Quoted() extends GlobalDirective("quoted")

case class TotalColumns(numberOfColumns: BigInt) extends GlobalDirective("totalColumns")

case class PermitEmpty() extends GlobalDirective("permitEmpty")

case class NoHeader() extends GlobalDirective("noHeader")

case class IgnoreColumnNameCase() extends GlobalDirective("ignoreColumnNameCase")

trait ColumnIdentifier {
  val value: String
  override def toString = value
}

case class NamedColumnIdentifier(name: String) extends ColumnIdentifier {
  val value = name
}

case class OffsetColumnIdentifier(offset: BigInt) extends ColumnIdentifier {
  val value = offset.toString
}

case class ColumnDefinition(id: ColumnIdentifier, rules: List[Rule] = Nil, directives: List[ColumnDirective] = Nil) extends Positional

trait ArgProvider {

  def referenceValue(columnIndex: Int, row: Row, schema: Schema): Option[String]

  def toError: String
}

case class ColumnReference(ref: ColumnIdentifier) extends ArgProvider {

  def referenceValue(columnIndex: Int, row: Row, schema: Schema): Option[String] = {
    val referencedIndex = schema.columnDefinitions.indexWhere(_.id == ref)
    row.cells.lift(referencedIndex).map(_.value)
  }

  @throws[IndexOutOfBoundsException]
  def referenceValueEx(columnIndex: Int, row: Row, schema: Schema): String = {
    referenceValue(columnIndex, row, schema).getOrElse{
      throw new ArrayIndexOutOfBoundsException(s"Could not access reference column $ref at [$columnIndex:${row.lineNumber}]")
    }
  }

  def toError ="$" + ref
}

case class Literal(value: Option[String]) extends ArgProvider {

  def referenceValue(columnIndex: Int, row: Row, schema: Schema): Option[String] = value

  def toError = value.map("\"" + _ + "\"").getOrElse("")
}



trait ColumnDirective extends Positional

case class Optional() extends ColumnDirective {
  override def toString = "optional"
}

case class MatchIsFalse() extends ColumnDirective {
  override def toString = "matchIsFalse"
}

case class Warning() extends ColumnDirective {
  override def toString = "warning"
}

case class IgnoreCase() extends ColumnDirective  {
  override def toString = "ignoreCase"
}
