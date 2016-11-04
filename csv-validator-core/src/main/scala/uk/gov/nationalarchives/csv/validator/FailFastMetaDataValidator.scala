/**
 * Copyright (c) 2013, The National Archives <digitalpreservation@nationalarchives.gov.uk>
 * http://www.nationalarchives.gov.uk
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package uk.gov.nationalarchives.csv.validator

import scalaz._
import Scalaz._
import annotation.tailrec
import scalaz.Failure
import uk.gov.nationalarchives.csv.validator.schema.ColumnDefinition
import uk.gov.nationalarchives.csv.validator.schema.Optional
import uk.gov.nationalarchives.csv.validator.schema.Rule
import uk.gov.nationalarchives.csv.validator.schema.Schema
import uk.gov.nationalarchives.csv.validator.schema.Warning
import uk.gov.nationalarchives.csv.validator.metadata.Cell
import uk.gov.nationalarchives.csv.validator.metadata.Row

trait FailFastMetaDataValidator extends MetaDataValidator {

  //TODO(AR) work on removing use of `Any`

  override def validateRows(rows: Iterator[Row], schema: Schema): MetaDataValidation[Any] = {

    @tailrec
    def validateRows(results: List[MetaDataValidation[Any]] = List.empty[MetaDataValidation[Any]]) : List[MetaDataValidation[Any]] = {
      if(results.headOption.map(containsErrors(_)).getOrElse(false) || !rows.hasNext) {
        results.reverse
      } else {
        val row = rows.next()
        val result = validateRow(row, schema, Some(rows.hasNext))
        /*
        **  Only store the results if they contain a warning or a failure.  This means the validator is not limited by the available memory 
        **  when processing large files.
        */
        if (containsErrors(result) || containsWarnings(result) ) {           
          validateRows(result :: results)
         } else {
            validateRows(results)
        }
      }
    }

    val v = validateRows()
    v.sequence[MetaDataValidation, Any]
  }

  override protected def rules(row: Row,  schema: Schema, mayBeLast: Option[Boolean] = None): MetaDataValidation[List[Any]] = {
    val cells: (Int) => Option[Cell] = row.cells.lift

    @tailrec
    def validateRules(columnDefinitions: List[(ColumnDefinition, Int)], accum: List[MetaDataValidation[Any]] = List.empty) : List[MetaDataValidation[Any]] = {
      columnDefinitions match {
        case Nil if(accum.isEmpty) =>
          List(true.successNel[FailMessage])

        case Nil if(accum.nonEmpty) =>
          accum.reverse

        case (columnDefinition, columnIndex) :: tail =>
          validateCell(columnIndex, cells, row, schema, mayBeLast) match {
            case failure @ Failure(_) if(!schema.columnDefinitions(columnIndex).directives.contains(Warning())) =>
              validateRules(List.empty, failure :: accum) //stop on first failure which is not a warning
            case result =>
              validateRules(tail, result :: accum)
          }
      }
    }

    val v = validateRules(schema.columnDefinitions.zipWithIndex)
    v.sequence[MetaDataValidation, Any]
  }

  override protected def rulesForCell(columnIndex: Int, row: Row,  schema: Schema, mayBeLast: Option[Boolean] = None): MetaDataValidation[Any] = {
    val columnDefinition = schema.columnDefinitions(columnIndex)

    def isWarningDirective: Boolean = columnDefinition.directives.contains(Warning())
    def isOptionDirective: Boolean = columnDefinition.directives.contains(Optional())

    @tailrec
    def validateRulesForCell(rules: List[Rule]): MetaDataValidation[Any] = rules match {
      case Nil => true.successNel[FailMessage]
      case rule :: tail => rule.evaluate(columnIndex, row, schema, mayBeLast) match {
        case e@Failure(_) => toErrors(e, row.lineNumber, columnIndex)
        case _ => validateRulesForCell(tail)
      }
    }

    def validateAllRulesForCell(rules: List[Rule]): MetaDataValidation[Any] = rules.map(_.evaluate(columnIndex, row, schema, mayBeLast)).map(toWarnings(_, row.lineNumber, columnIndex)).sequence[MetaDataValidation, Any]

    if(row.cells(columnIndex).value.trim.isEmpty && isOptionDirective) true.successNel
    else if(isWarningDirective) validateAllRulesForCell(columnDefinition.rules)
    else validateRulesForCell(columnDefinition.rules)
  }
}
