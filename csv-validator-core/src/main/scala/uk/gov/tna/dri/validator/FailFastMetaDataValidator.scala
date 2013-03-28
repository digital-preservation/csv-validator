/*
 * Copyright (c) 2013, The National Archives digitalpreservation@nationalarchives.gov.uk
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package uk.gov.tna.dri.validator

import uk.gov.tna.dri.schema._
import scalaz._
import Scalaz._
import uk.gov.tna.dri.metadata.Cell
import uk.gov.tna.dri.metadata.Row
import uk.gov.tna.dri.schema.ColumnDefinition
import uk.gov.tna.dri.schema.Schema
import scala.Some
import uk.gov.tna.dri.schema.Optional
import annotation.tailrec

trait FailFastMetaDataValidator extends MetaDataValidator {

  val pathSubstitutions: List[(String,String)]

  def validateRows(rows: List[Row], schema: Schema): MetaDataValidation[Any] = {

    def containsErrors(e:MetaDataValidation[Any]): Boolean = e.fail.exists(a => a.list.exists(i => i.isInstanceOf[ErrorMessage]))

//    @tailrec
    def validateRows(rows: List[Row]): MetaDataValidation[Any] = rows match {
      case Nil => true.successNel[FailMessage]
      case r :: tail =>  validateRow(r, schema) match {
        case e@Failure(_) =>
          if( containsErrors(e)) e else e *> validateRows(tail)
        case _ => validateRows(tail)
      }
    }

    validateRows(rows)
  }

  private def validateRow(row: Row, schema: Schema): MetaDataValidation[Any] = {
    totalColumns(row, schema).fold(e => e.fail[Any], s => rules(row, schema))
  }

  private def totalColumns(row: Row, schema: Schema): MetaDataValidation[Any] = {
    val tc: Option[TotalColumns] = schema.globalDirectives.collectFirst{ case t@TotalColumns(_) => t }

    if (tc.isEmpty || tc.get.numberOfColumns == row.cells.length) true.successNel[FailMessage]
    else ErrorMessage(s"Expected @totalColumns of ${tc.get.numberOfColumns} and found ${row.cells.length} on line ${row.lineNumber}").failNel[Any]
  }

  private def rules(row: Row, schema: Schema): MetaDataValidation[Any] = {
    val cells: (Int) => Option[Cell] = row.cells.lift

//    @tailrec
    def validateRules(columnDefinitions:List[(ColumnDefinition,Int)]): MetaDataValidation[Any] = columnDefinitions match {
      case Nil => true.successNel[FailMessage]
      case (columnDef, columnIndex) :: tail => validateCell(columnIndex, cells, row, schema) match {
        case e@Failure(_) =>
          if( schema.columnDefinitions(columnIndex).directives.contains(Warning()))  e  *> validateRules(tail)
          else e
        case _ => validateRules(tail)
      }
    }
    validateRules(schema.columnDefinitions.zipWithIndex)
  }

  private def validateCell(columnIndex: Int, cells: (Int) => Option[Cell], row: Row, schema: Schema): MetaDataValidation[Any] = {
    cells(columnIndex) match {
      case Some(c) => rulesForCell(columnIndex, row, schema)
      case _ => SchemaMessage(s"Missing value at line: ${row.lineNumber}, column: ${schema.columnDefinitions(columnIndex).id}").failNel[Any]
    }
  }

  private def rulesForCell(columnIndex: Int, row: Row, schema: Schema): MetaDataValidation[Any] = {
    val columnDefinition = schema.columnDefinitions(columnIndex)

    def isWarningDirective: Boolean = columnDefinition.directives.contains(Warning())
    def isOptionDirective: Boolean = columnDefinition.directives.contains(Optional())

    def convert2Warnings( results:Rule#RuleValidation[Any]): MetaDataValidation[Any] = {
      results.fail.map{errorList => errorList.map(errorText => WarningMessage(errorText))}.validation
    }

    def convert2Errors( results:Rule#RuleValidation[Any]): MetaDataValidation[Any] = {
      results.fail.map{errorList => errorList.map(errorText => ErrorMessage(errorText))}.validation
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