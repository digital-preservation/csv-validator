package uk.gov.tna.dri.schema

import scalaz.{Success => SuccessZ, Failure => FailureZ}

import scalaz.Scalaz._
import scala.collection.mutable
import uk.gov.tna.dri.metadata.Row


case class UniqueRule() extends Rule("unique") {
  val distinctValues = mutable.HashMap[String, Int]()

  override def evaluate(columnIndex: Int, row: Row, schema: Schema): RuleValidation[Any] = {
    val columnDefinition = schema.columnDefinitions(columnIndex)

    def originalValue: Option[String] = {
      val cellValue = cellValueCorrectCase
      if (distinctValues contains cellValue) Some(cellValue) else None
    }

    def cellValueCorrectCase = if (columnDefinition.directives contains IgnoreCase()) cellValue(columnIndex,row,schema).toLowerCase else cellValue(columnIndex,row,schema)

    originalValue match {
      case None => distinctValues.put(cellValueCorrectCase, row.lineNumber); true.successNel
      case Some(o) => {
        s"$toError fails for line: ${row.lineNumber}, column: ${columnDefinition.id}, ${toValueError(row,columnIndex)} (original at line: ${distinctValues(o)})".failNel[Any]
      }
    }
  }

  def valid(cellValue: String, columnDefinition: ColumnDefinition, columnIndex: Int, row: Row, schema: Schema): Boolean = {
    evaluate(columnIndex, row, schema) match {
      case FailureZ(_) => false
      case SuccessZ(_) => true
    }
  }
}

case class UniqueMultiRule( columns: List[String] ) extends Rule("unique(") {
  val SEPARATOR:Char = 0x07 // BEL
  val distinctValues = mutable.HashMap[String, Int]()

  override def evaluate(columnIndex: Int, row: Row, schema: Schema): RuleValidation[Any] = {
    val columnDefinition = schema.columnDefinitions(columnIndex)

    def secondaryValues: String =  columns.foldLeft(""){ case (s,c) => s + SEPARATOR + row.cells(columnNameToIndex(schema, c)).value }

    def uniqueString: String =  cellValue(columnIndex,row,schema) + SEPARATOR +  secondaryValues

    def originalValue: Option[String] = {
      val cellValue = cellValueCorrectCase
      if (distinctValues contains cellValue) Some(cellValue) else None
    }

    def cellValueCorrectCase = if (columnDefinition.directives contains IgnoreCase) uniqueString.toLowerCase else uniqueString

    originalValue match {
      case None => distinctValues.put(cellValueCorrectCase, row.lineNumber); true.successNel
      case Some(o) => {
        s"$toError ${columns.mkString("$", ", $", "")} ) fails for line: ${row.lineNumber}, column: ${columnDefinition.id}, ${toValueError(row,columnIndex)} (original at line: ${distinctValues(o)})".failNel[Any]
      }
    }
  }

  def valid(cellValue: String, columnDefinition: ColumnDefinition, columnIndex: Int, row: Row, schema: Schema): Boolean = {
    evaluate(columnIndex, row, schema) match {
      case FailureZ(_) => false
      case SuccessZ(_) => true
    }
  }
}
