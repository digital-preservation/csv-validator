package uk.gov.tna.dri.schema

import util.matching.Regex
import scalaz._
import Scalaz._

sealed trait Rule {

  def execute(str: String): ValidationNEL[String, Boolean]
}

case class RegexRule(regex: Regex) extends Rule {

  override def execute(str: String): ValidationNEL[String, Boolean] = {
    val exp = regex.pattern.pattern
    if (str matches exp) true.successNel[String] else s"Value: ${str} does not match regex: ${exp}".failNel[Boolean]
  }
}
