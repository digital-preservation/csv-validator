package uk.gov.tna.dri.schema

import util.matching.Regex
import scalaz._
import Scalaz._

sealed trait Rule {

  def execute(rowIndex: Int, columnToValueMap: Map[String,String], colDef: ColumnDefinition, value: String): ValidationNEL[String, Any]
}

case class RegexRule(regex: Regex) extends Rule {

  override def execute(lineNumber: Int, columnToValueMap: Map[String,String], colDef: ColumnDefinition, value: String): ValidationNEL[String, Any] = {
    val exp = regex.pattern.pattern
    if (value matches exp) true.successNel[String] else s"regex: ${exp} fails for line ${lineNumber}, column: ${colDef.id}".failNel[Any]
  }
}

case class InRule(inVal: StringProvider) extends Rule {
  override def execute(rowIndex: Int, columnToValueMap: Map[String,String], colDef: ColumnDefinition, value: String): ValidationNEL[String, Any] = {
    val colVal = inVal.getColumnValue(columnToValueMap)

    if (value.contains(colVal)) true.successNel[String] else s"inRule: ${colVal} fails for line ${rowIndex+1}, column: ${colDef.id}, value: ${value}".failNel[Any]
  }
}
