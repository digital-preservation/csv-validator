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

  def validateRows(rows: List[Row], schema: Schema): FailMetaDataValidation[Any] = {

    @tailrec
    def validateRows(rows: List[Row]): FailMetaDataValidation[Any] = rows match {
      case Nil => true.successNel[FailMessage]
      case r :: tail =>  validateRow(r, schema) match {
        case e@Failure(_) => e
        case _ => validateRows(tail)
      }
    }

    validateRows(rows)
  }

  private def validateRow(row: Row, schema: Schema): FailMetaDataValidation[Any] = {
    totalColumns(row, schema).fold(e => e.fail[Any], s => rules(row, schema))
  }

  private def totalColumns(row: Row, schema: Schema): FailMetaDataValidation[Any] = {
    val tc: Option[TotalColumns] = schema.globalDirectives.collectFirst{ case t@TotalColumns(_) => t }

    if (tc.isEmpty || tc.get.numberOfColumns == row.cells.length) true.successNel[FailMessage]
    else ErrorMessage(s"Expected @totalColumns of ${tc.get.numberOfColumns} and found ${row.cells.length} on line ${row.lineNumber}").failNel[Any]
  }

  private def rules(row: Row, schema: Schema): FailMetaDataValidation[Any] = {
    val cells: (Int) => Option[Cell] = row.cells.lift

    @tailrec
    def validateRules(columnDefinitions:List[(ColumnDefinition,Int)]): FailMetaDataValidation[Any] = columnDefinitions match {
      case Nil => true.successNel[FailMessage]
      case (columnDef, columnIndex) :: tail => validateCell(columnIndex, cells, row, schema) match {
        case e@Failure(_) => e
        case _ => validateRules(tail)
      }
    }
    validateRules(schema.columnDefinitions.zipWithIndex)
  }

  private def validateCell(columnIndex: Int, cells: (Int) => Option[Cell], row: Row, schema: Schema): FailMetaDataValidation[Any] = {
    cells(columnIndex) match {
      case Some(c) => rulesForCell(columnIndex, row, schema)
      case _ => ErrorMessage(s"Missing value at line: ${row.lineNumber}, column: ${schema.columnDefinitions(columnIndex).id}").failNel[Any]
    }
  }

  private def rulesForCell(columnIndex: Int, row: Row, schema: Schema): FailMetaDataValidation[Any] = {
    val columnDefinition = schema.columnDefinitions(columnIndex)

    def isWarningDirective: Boolean = columnDefinition.directives.contains(Warning())
    def isOptionDirective: Boolean = columnDefinition.directives.contains(Optional())

    def convert2Warnings( results:Rule#RuleValidation[Any]): FailMetaDataValidation[Any] = {
      results.fail.map{errorList => errorList.map(errorText => ErrorMessage("Warning: " + errorText))}.validation
    }

    def convert2Errors( results:Rule#RuleValidation[Any]): FailMetaDataValidation[Any] = {
      results.fail.map{errorList => errorList.map(errorText => ErrorMessage("Error: " + errorText))}.validation
    }

    @tailrec
    def validateRulesForCell(rules:List[Rule]): FailMetaDataValidation[Any] = rules match {
      case Nil => true.successNel[FailMessage]
      case rule :: tail => rule.evaluate(columnIndex, row, schema) match {
        case e@Failure(_) => if(isWarningDirective) convert2Warnings(e) else convert2Errors(e)
        case _ => validateRulesForCell(tail)
      }
    }

    if (row.cells(columnIndex).value.trim.isEmpty && isOptionDirective ) true.successNel
    else validateRulesForCell(columnDefinition.rules)
  }
}