package uk.gov.tna.dri.schema

import scalaz._
import Scalaz._
import uk.gov.tna.dri.metadata.Row

trait Rule {
  def execute(columnIndex: Int, row: Row, schema: Schema): ValidationNEL[String, Any]
}

abstract class StringProviderRule(inVal: StringProvider) {

  def runRule(columnIndex: Int, row: Row, schema: Schema, matcher: (String, String) => Boolean): ValidationNEL[String, Any] = {
    val columnDefinition = schema.columnDefinitions(columnIndex)

    inVal.referenceValue(columnIndex, row, schema) match {
      case Left(e) => e.failNel[Any]

      case Right(ruleValue) => {
        val cellValue = row.cells(columnIndex).value
        val (cv, rv) = if (schema.columnDefinitions(columnIndex).contains(IgnoreCase())) (cellValue.toLowerCase, ruleValue.toLowerCase) else (cellValue, ruleValue)

        if (matcher(rv,cv))
          true.successNel[String]
        else
          s"in: ${ruleValue} fails for line ${row.lineNumber}, column: ${columnDefinition.id}, value: ${cellValue}".failNel[Any]
      }
    }
  }
}

case class IsRule(inVal: StringProvider) extends StringProviderRule(inVal) with Rule  {

  override def execute(columnIndex: Int, row: Row, schema: Schema): ValidationNEL[String, Any] = {
    runRule(columnIndex, row, schema, _.equals(_))
  }
}

case class NotRule(inVal: StringProvider) extends StringProviderRule(inVal) with Rule  {

  override def execute(columnIndex: Int, row: Row, schema: Schema): ValidationNEL[String, Any] = {
    runRule(columnIndex, row, schema, !_.equals(_))
  }
}

case class InRule(inVal: StringProvider) extends StringProviderRule(inVal) with Rule  {

  override def execute(columnIndex: Int, row: Row, schema: Schema): ValidationNEL[String, Any] = {
    runRule(columnIndex, row, schema, _.contains(_))
  }
}

case class StartsRule(inVal: StringProvider) extends StringProviderRule(inVal) with Rule  {

  override def execute(columnIndex: Int, row: Row, schema: Schema): ValidationNEL[String, Any] = {
    runRule(columnIndex, row, schema, _.startsWith(_))
  }
}

case class EndsRule(inVal: StringProvider) extends StringProviderRule(inVal) with Rule {

  override def execute(columnIndex: Int, row: Row, schema: Schema): ValidationNEL[String, Any] = {
    runRule(columnIndex, row, schema, _.endsWith(_))
  }
}