/**
 * Copyright (c) 2013, The National Archives <digitalpreservation@nationalarchives.gov.uk>
 * https://www.nationalarchives.gov.uk
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package uk.gov.nationalarchives.csv.validator

import scalaz._, Scalaz._
import uk.gov.nationalarchives.csv.validator.schema.Optional
import uk.gov.nationalarchives.csv.validator.schema.Rule
import uk.gov.nationalarchives.csv.validator.schema.Schema
import uk.gov.nationalarchives.csv.validator.schema.Warning
import uk.gov.nationalarchives.csv.validator.metadata.Cell
import uk.gov.nationalarchives.csv.validator.metadata.Row
import scala.annotation.tailrec

trait AllErrorsMetaDataValidator extends MetaDataValidator {

  override def validateRows(rows: Iterator[Row], schema: Schema): MetaDataValidation[Any] = {

    @tailrec
    def validateRows(results: List[MetaDataValidation[Any]] = List.empty[MetaDataValidation[Any]]) : List[MetaDataValidation[Any]] = {
      if(!rows.hasNext) {
        results.reverse
      } else {
        val row = rows.next()
        val result = validateRow(row, schema, Some(rows.hasNext))
        /*
        Only store the results if they contain a warning or a failure.  This means the validator is not limited by the
        available memory when processing large files.
         */
        if (containsErrors(result) || containsWarnings(result)) {
            validateRows(result :: results)
         } else {
            validateRows(results)
        }
      }
    }

    val v = validateRows()
    v.sequence[MetaDataValidation, Any]
  }


  override protected def rules(row: Row, schema: Schema, mayBeLast: Option[Boolean] = None): MetaDataValidation[List[Any]] = {
    val cells: (Int) => Option[Cell] = row.cells.lift
    val v = schema.columnDefinitions.zipWithIndex.map {
      case (columnDefinition, columnIndex) =>
        validateCell(columnIndex, cells, row, schema, mayBeLast)
    }

    v.sequence[MetaDataValidation, Any]
  }

  override protected def rulesForCell(columnIndex: Int, row: Row, schema: Schema, mayBeLast: Option[Boolean] = None): MetaDataValidation[Any] = {

    val columnDefinition = schema.columnDefinitions(columnIndex)

    def isWarningDirective: Boolean = columnDefinition.directives.contains(Warning())
    def isOptionDirective: Boolean = columnDefinition.directives.contains(Optional())

    if(row.cells(columnIndex).value.trim.isEmpty && isOptionDirective) true.successNel
    else columnDefinition.rules.map{rule =>
      rule.evaluate(columnIndex, row, schema, mayBeLast)
    }.map{ ruleResult:Rule#RuleValidation[Any] => {
      if(isWarningDirective) toWarnings(ruleResult, row.lineNumber, columnIndex) else toErrors(ruleResult, row.lineNumber, columnIndex)
    }}.sequence[MetaDataValidation, Any]
  }
}
