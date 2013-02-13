package uk.gov.tna.dri.schema

import scalaz._
import Scalaz._
import uk.gov.tna.dri.metadata.Row

trait Rule {

  val name: String

  def evaluate(columnIndex: Int, row: Row, schema: Schema): ValidationNEL[String, Any]

  def error(ruleValue: String, columnIndex: Int, row: Row, schema: Schema) = {
    val columnDefinition = schema.columnDefinitions(columnIndex)
    s"${name}: ${ruleValue} fails for line: ${row.lineNumber}, column: ${columnDefinition.id}, value: ${row.cells(columnIndex).value}".failNel[Any]
  }
}

abstract class StringProviderRule(inVal: StringProvider) {

  def evaluate(columnIndex: Int, row: Row, schema: Schema, matching: (String, String) => Boolean): ValidationNEL[String, Any] = {
    val columnDefinition = schema.columnDefinitions(columnIndex)
    val cellValue = row.cells(columnIndex).value
    val ruleValue = inVal.referenceValue(columnIndex, row, schema)

    val (cv, rv) = if (schema.columnDefinitions(columnIndex).contains(IgnoreCase())) (cellValue.toLowerCase, ruleValue.toLowerCase) else (cellValue, ruleValue)

    if (matching(rv, cv))
      true.successNel[String]
    else
      s"in: ${ruleValue} fails for line ${row.lineNumber}, column: ${columnDefinition.id}, value: ${cellValue}".failNel[Any]
  }
}

case class IsRule(inVal: StringProvider) extends StringProviderRule(inVal) with Rule  {

  override val name = "is"

  override def evaluate(columnIndex: Int, row: Row, schema: Schema): ValidationNEL[String, Any] = {
    evaluate(columnIndex, row, schema, _.equals(_))
  }
}

case class NotRule(inVal: StringProvider) extends StringProviderRule(inVal) with Rule  {

  override val name = "not"

  override def evaluate(columnIndex: Int, row: Row, schema: Schema): ValidationNEL[String, Any] = {
    evaluate(columnIndex, row, schema, !_.equals(_))
  }
}

case class InRule(inVal: StringProvider) extends StringProviderRule(inVal) with Rule  {

  override val name = "in"

  override def evaluate(columnIndex: Int, row: Row, schema: Schema): ValidationNEL[String, Any] = {
    evaluate(columnIndex, row, schema, _.contains(_))
  }
}

case class StartsRule(inVal: StringProvider) extends StringProviderRule(inVal) with Rule  {

  override val name = "starts"

  override def evaluate(columnIndex: Int, row: Row, schema: Schema): ValidationNEL[String, Any] = {
    evaluate(columnIndex, row, schema, _.startsWith(_))
  }
}

case class EndsRule(inVal: StringProvider) extends StringProviderRule(inVal) with Rule {

  override val name = "ends"

  override def evaluate(columnIndex: Int, row: Row, schema: Schema): ValidationNEL[String, Any] = {
    evaluate(columnIndex, row, schema, _.endsWith(_))
  }
}