package uk.gov.tna.dri.schema

import util.parsing.combinator._

object SchemaParser extends RegexParsers {

  def totalColumns = "@TotalColumns : " ~> positiveNumber ^^ { _.toInt }

  def positiveNumber = """\d+""".r
}
