/*
 * Copyright (c) 2013, The National Archives digitalpreservation@nationalarchives.gov.uk
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package uk.gov.tna.dri.validator

import uk.gov.tna.dri.schema._
import scalaz._, Scalaz._
import uk.gov.tna.dri.metadata.Cell
import uk.gov.tna.dri.metadata.Row
import uk.gov.tna.dri.schema.TotalColumns
import scala.Some
import uk.gov.tna.dri.schema.Optional
import scalaz.Digit._3

trait AllErrorsMetaDataValidator extends MetaDataValidator {

  def validateRows(rows: List[Row], schema: Schema): MetaDataValidation[Any] = {
    val v = for (row <- rows) yield validateRow(row, schema)
    v.sequence[MetaDataValidation, Any]
  }

  private def validateRow(row: Row, schema: Schema): MetaDataValidation[Any] = {
    val totalColumnsV = totalColumns(row, schema)
    val rulesV = rules(row, schema)
    (totalColumnsV |@| rulesV) { _ :: _ }
  }

  private def totalColumns(row: Row, schema: Schema): MetaDataValidation[Any] = {
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

  private def validateCell(columnIndex: Int, cells: (Int) => Option[Cell], row: Row, schema: Schema): MetaDataValidation[Any] = {
    cells(columnIndex) match {
      case Some(c) => rulesForCell(columnIndex, row, schema)
      case _ => ErrorMessage(s"Missing value at line: ${row.lineNumber}, column: ${schema.columnDefinitions(columnIndex).id}").failNel[Any]
    }
  }

  private def rulesForCell(columnIndex: Int, row: Row, schema: Schema): MetaDataValidation[Any] = {

    val columnDefinition = schema.columnDefinitions(columnIndex)

    def isWarningDirective: Boolean = columnDefinition.directives.contains(Warning())
    def isOptionDirective: Boolean = columnDefinition.directives.contains(Optional())

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
}