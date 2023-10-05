/*
 * Copyright (c) 2013, The National Archives <digitalpreservation@nationalarchives.gov.uk>
 * https://www.nationalarchives.gov.uk
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package uk.gov.nationalarchives.csv.validator

import cats.data.Validated.{Invalid => Failure}
import cats.syntax.all._
import uk.gov.nationalarchives.csv.validator.metadata.{Cell, Row}
import uk.gov.nationalarchives.csv.validator.schema._

import scala.annotation.tailrec

trait FailFastMetaDataValidator extends MetaDataValidator {

  //TODO(AR) work on removing use of `Any`

  override def validateRows(
    rows: Iterator[Row],
    schema: Schema,
    rowCallback: MetaDataValidation[Any] => Unit
  ): Boolean = {

    @tailrec
    def inner(passing: Boolean) : Boolean = {
      if(!rows.hasNext) {
        passing
      } else {
        val row = rows.next()
        val result = validateRow(row, schema, Some(rows.hasNext))
        rowCallback(result)
        if(!containsErrors(result))
          inner(passing)
        else 
          false
      }
    }

    inner(true)
  }

  override protected def rules(row: Row,  schema: Schema, mayBeLast: Option[Boolean] = None): MetaDataValidation[List[Any]] = {
    val cells: (Int) => Option[Cell] = row.cells.lift

    @tailrec
    def validateRules(columnDefinitions: List[(ColumnDefinition, Int)], accum: List[MetaDataValidation[Any]] = List.empty) : List[MetaDataValidation[Any]] = {
      columnDefinitions match {
        case Nil =>
          if (accum.isEmpty) {
            List(true.validNel[FailMessage])
          } else {
            accum.reverse
          }

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
      case Nil => true.validNel[FailMessage]
      case rule :: tail => rule.evaluate(columnIndex, row, schema, mayBeLast) match {
        case e@Failure(_) => toErrors(e, row.lineNumber, columnIndex)
        case _ => validateRulesForCell(tail)
      }
    }

    def validateAllRulesForCell(rules: List[Rule]): MetaDataValidation[Any] = rules.map(_.evaluate(columnIndex, row, schema, mayBeLast)).map(toWarnings(_, row.lineNumber, columnIndex)).sequence[MetaDataValidation, Any]

    if(row.cells(columnIndex).value.trim.isEmpty && isOptionDirective) true.validNel
    else if(isWarningDirective) validateAllRulesForCell(columnDefinition.rules)
    else validateRulesForCell(columnDefinition.rules)
  }
}
