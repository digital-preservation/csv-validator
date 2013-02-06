package uk.gov.tna.dri.schema

import util.matching.Regex
import scalaz._
import Scalaz._

sealed trait Rule {

  def execute(rowIndex: Int, colDef: ColumnDefinition, value: String): ValidationNEL[String, Any]
}

case class RegexRule(regex: Regex) extends Rule {

  override def execute(lineNumber: Int, colDef: ColumnDefinition, value: String): ValidationNEL[String, Any] = {
    val exp = regex.pattern.pattern
    if (value matches exp) true.successNel[String] else s"regex: ${exp} fails for line ${lineNumber}, column: ${colDef.id}".failNel[Any]
  }
}

case class InRule(inVal: String) extends Rule {
  override def execute(lineNumber: Int, colDef: ColumnDefinition, value: String): ValidationNEL[String, Any] = {
    if (value.contains(inVal)) true.successNel[String] else s"inRule: ${inVal} fails for line ${lineNumber}, column: ${colDef.id}, value: ${value}".failNel[Any]
  }
}
