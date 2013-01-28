package uk.gov.tna.dri.schema

import util.matching.Regex

case class Schema(totalColumns: Int, regex: Option[Regex] = None)
