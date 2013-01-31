package uk.gov.tna.dri.schema

import util.matching.Regex
import scalaz._
import Scalaz._

sealed trait Rule {

  def execute(value: String): ValidationNEL[String, Boolean]
}

case class RegexRule(regex: Regex) extends Rule {

  override def execute(value: String): ValidationNEL[String, Boolean] = {
    val exp = regex.pattern.pattern
    if (value matches exp) true.successNel[String] else s"Value: ${value} does not match regex: ${exp}".failNel[Boolean]
  }
}
