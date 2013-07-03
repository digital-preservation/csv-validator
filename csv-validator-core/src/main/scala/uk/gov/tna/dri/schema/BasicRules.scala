package uk.gov.tna.dri.schema

import scalaz.{Success => SuccessZ, Failure => FailureZ}

import scala.Some
import uk.gov.tna.dri.metadata.Row
import scala.util.Try

case class RegexRule(regex: String) extends Rule("regex") {
  def valid(cellValue: String, columnDefinition: ColumnDefinition, columnIndex: Int, row: Row, schema: Schema): Boolean = {

    val regexp = if (columnDefinition.directives.contains(IgnoreCase())) "(?i)" + regex else regex
    cellValue matches regexp
  }

  override def toError = {
    s"""$ruleName("$regex")"""
  }
}

case class InRule(inValue: ArgProvider) extends Rule("in", inValue) {
  def valid(cellValue: String, columnDefinition: ColumnDefinition, columnIndex: Int, row: Row, schema: Schema): Boolean = {
    val ruleValue = inValue.referenceValue(columnIndex, row, schema)

    val (rv, cv) = if (columnDefinition.directives.contains(IgnoreCase())) (ruleValue.get.toLowerCase, cellValue.toLowerCase) else (ruleValue.get, cellValue)
    rv contains cv
  }
}

case class IsRule(isValue: ArgProvider) extends Rule("is", isValue) {
  def valid(cellValue: String, columnDefinition: ColumnDefinition, columnIndex: Int, row: Row, schema: Schema): Boolean = {
    val ruleValue = isValue.referenceValue(columnIndex, row, schema)

    val (rv, cv) = if (columnDefinition.directives.contains(IgnoreCase())) (ruleValue.get.toLowerCase, cellValue.toLowerCase) else (ruleValue.get, cellValue)
    cv == rv
  }
}

case class IsNotRule(isNotValue: ArgProvider) extends Rule("isNot", isNotValue) {
  def valid(cellValue: String, columnDefinition: ColumnDefinition, columnIndex: Int, row: Row, schema: Schema): Boolean = {
    val ruleValue = isNotValue.referenceValue(columnIndex, row, schema)

    val (rv, cv) = if (columnDefinition.directives.contains(IgnoreCase())) (ruleValue.get.toLowerCase, cellValue.toLowerCase) else (ruleValue.get, cellValue)
    cv != rv
  }
}

case class StartsRule(startsValue: ArgProvider) extends Rule("starts", startsValue) {
  def valid(cellValue: String, columnDefinition: ColumnDefinition, columnIndex: Int, row: Row, schema: Schema): Boolean = {
    val ruleValue = startsValue.referenceValue(columnIndex, row, schema)

    val (rv, cv) = if (columnDefinition.directives.contains(IgnoreCase())) (ruleValue.get.toLowerCase, cellValue.toLowerCase) else (ruleValue.get, cellValue)
    cv startsWith rv
  }
}

case class EndsRule(endsValue: ArgProvider) extends Rule("ends", endsValue) {
  def valid(cellValue: String, columnDefinition: ColumnDefinition, columnIndex: Int, row: Row, schema: Schema): Boolean = {
    val ruleValue = endsValue.referenceValue(columnIndex, row, schema)

    val (rv, cv) = if (columnDefinition.directives.contains(IgnoreCase())) (ruleValue.get.toLowerCase, cellValue.toLowerCase) else (ruleValue.get, cellValue)
    cv endsWith rv
  }
}

case class UriRule() extends PatternRule("uri", UriRegex)

case class Uuid4Rule() extends PatternRule("uuid4", Uuid4Regex)

case class PositiveIntegerRule() extends PatternRule("positiveInteger", PositiveIntegerRegex)

case class RangeRule(min: BigDecimal, max: BigDecimal) extends Rule("range") {
  def valid(cellValue: String, columnDefinition: ColumnDefinition, columnIndex: Int, row: Row, schema: Schema): Boolean = {

    Try[BigDecimal]( BigDecimal(cellValue)) match {
      case scala.util.Success(callDecimal) => if (callDecimal >= min && callDecimal <= max  ) true  else false
      case _ => false
     }
  }

  override def toError = s"""$ruleName($min,$max)"""
}

case class LengthRule(from: Option[String], to: String) extends Rule("length") {

  def toValue: Int = if (to == "*") Int.MaxValue else to.toInt
  def fromValue: Int =  if (from.get == "*") 0 else from.get.toInt

  def valid(cellValue: String, columnDefinition: ColumnDefinition, columnIndex: Int, row: Row, schema: Schema): Boolean = {
    val cellLen = cellValue.length

    from match {
      case None => if ( to=="*") true else cellLen == to.toInt
      case Some(_) => cellLen >= fromValue && cellLen <= toValue
    }
  }

  override def toError = if(from.isDefined) s"""$ruleName(${from.get},$to)""" else s"""$ruleName($to)"""
}

