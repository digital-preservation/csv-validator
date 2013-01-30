package uk.gov.tna.dri.schema

import util.matching.Regex

sealed trait Rule

case class RegexRule(regex: Regex) extends Rule

case class NonEmptyRule() extends Rule
