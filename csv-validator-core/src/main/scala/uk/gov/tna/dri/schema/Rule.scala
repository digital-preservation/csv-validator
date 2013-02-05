package uk.gov.tna.dri.schema

import util.matching.Regex
import scalaz._
import Scalaz._

sealed trait Rule {

  def execute(rowIndex: Int, colDef: ColumnDefinition, value: String): ValidationNEL[String, Boolean]
}

case class RegexRule(regex: Regex) extends Rule {

  override def execute(rowIndex: Int, colDef: ColumnDefinition, value: String): ValidationNEL[String, Boolean] = {
    val exp = regex.pattern.pattern
    if (value matches exp) true.successNel[String] else s"regex: ${exp} fails for line ${rowIndex+1}, column: ${colDef.id}".failNel[Boolean]
  }
}

case class InRule(inVal: String) extends Rule {
  override def execute(rowIndex: Int, colDef: ColumnDefinition, value: String): ValidationNEL[String, Boolean] = {
    if (value.contains(inVal)) true.successNel[String] else s"inRule: ${inVal} fails for line ${rowIndex+1}, column: ${colDef.id}, value: ${value}".failNel[Boolean]
  }
}
