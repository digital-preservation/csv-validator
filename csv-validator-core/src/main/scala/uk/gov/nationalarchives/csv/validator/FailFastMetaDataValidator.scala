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
import uk.gov.nationalarchives.csv.validator.schema.TotalColumns
import uk.gov.nationalarchives.csv.validator.schema.Warning
import uk.gov.nationalarchives.csv.validator.metadata.Cell
import uk.gov.nationalarchives.csv.validator.metadata.Row

trait FailFastMetaDataValidator extends MetaDataValidator {

  val pathSubstitutions: List[(String,String)]

  def validateRows(rows: Iterator[Row], schema: Schema): MetaDataValidation[Any] = {

    def containsErrors(e: MetaDataValidation[Any]): Boolean = e.fold(_.list.exists(_.isInstanceOf[ErrorMessage]), _ => false)

//  @tailrec
//    def validateRows(rows: Iterator[Row]): MetaDataValidation[Any] = rows match {
//      case Nil => true.successNel[FailMessage]
//      case r :: tail =>  validateRow(r, schema) match {
//        case e @ Failure(messages) =>
//          if( containsErrors(e)) e else validateRows(tail).leftMap(_ append messages)
//        case _ => validateRows(tail)
//      }
//    }

    def validateRows(): MetaDataValidation[Any] = {
      if(!rows.hasNext) {
        true.successNel[FailMessage]
      } else {
        val row = rows.next()
        validateRow(row, schema) match {
          case e @ Failure(messages) =>
            if(containsErrors(e)) {
              e
            } else {
              validateRows().leftMap(_ append messages)
            }
          case _ =>
            validateRows()
        }
      }
    }

    validateRows()
  }

  private def validateRow(row: Row, schema: Schema): MetaDataValidation[Any] = {
    totalColumns(row, schema).fold(e => e.fail[Any], s => rules(row, schema))
  }

  private def totalColumns(row: Row, schema: Schema): MetaDataValidation[Any] = {
    val tc: Option[TotalColumns] = schema.globalDirectives.collectFirst{ case t@TotalColumns(_) => t }

    if (tc.isEmpty || tc.get.numberOfColumns == row.cells.length) true.successNel[FailMessage]
    else ErrorMessage(s"Expected @totalColumns of ${tc.get.numberOfColumns} and found ${row.cells.length} on line ${row.lineNumber}", row.lineNumber, row.cells.length).failNel[Any]
  }

  private def rules(row: Row, schema: Schema): MetaDataValidation[Any] = {
    val cells: (Int) => Option[Cell] = row.cells.lift

//    @tailrec
    def validateRules(columnDefinitions:List[(ColumnDefinition,Int)]): MetaDataValidation[Any] = columnDefinitions match {
      case Nil => true.successNel[FailMessage]
      case (columnDef, columnIndex) :: tail => validateCell(columnIndex, cells, row, schema) match {
        case e@Failure(messages) =>
          if( schema.columnDefinitions(columnIndex).directives.contains(Warning())) validateRules(tail).leftMap(_ append messages)
          else e
        case _ => validateRules(tail)
      }
    }
    validateRules(schema.columnDefinitions.zipWithIndex)
  }

  private def validateCell(columnIndex: Int, cells: (Int) => Option[Cell], row: Row, schema: Schema): MetaDataValidation[Any] = {
    cells(columnIndex) match {
      case Some(c) => rulesForCell(columnIndex, row, schema)
      case _ => SchemaMessage(s"Missing value at line: ${row.lineNumber}, column: ${schema.columnDefinitions(columnIndex).id}", row.lineNumber, columnIndex).failNel[Any]
    }
  }

  private def rulesForCell(columnIndex: Int, row: Row, schema: Schema): MetaDataValidation[Any] = {
    val columnDefinition = schema.columnDefinitions(columnIndex)

    def isWarningDirective: Boolean = columnDefinition.directives.contains(Warning())
    def isOptionDirective: Boolean = columnDefinition.directives.contains(Optional())

    def convert2Warnings(results:Rule#RuleValidation[Any]): MetaDataValidation[Any] = {
      results.leftMap(_.map(WarningMessage(_, row.lineNumber, columnIndex)))
    }

    def convert2Errors(results:Rule#RuleValidation[Any]): MetaDataValidation[Any] = {
      results.leftMap(_.map(ErrorMessage(_, row.lineNumber, columnIndex)))
    }

    @tailrec
    def validateRulesForCell(rules:List[Rule]): MetaDataValidation[Any] = rules match {
      case Nil => true.successNel[FailMessage]
      case rule :: tail => rule.evaluate(columnIndex, row, schema) match {
        case e@Failure(_) => convert2Errors(e)
        case _ => validateRulesForCell(tail)
      }
    }

    def validateAllRulesForCell(rules:List[Rule]): MetaDataValidation[Any] = {
      rules.map(_.evaluate(columnIndex, row, schema)).map{ ruleResult:Rule#RuleValidation[Any] => {
        convert2Warnings(ruleResult)
      }}.sequence[MetaDataValidation, Any]
    }

    if (row.cells(columnIndex).value.trim.isEmpty && isOptionDirective ) true.successNel
    else if(isWarningDirective) validateAllRulesForCell(columnDefinition.rules)
    else validateRulesForCell(columnDefinition.rules)
  }
}