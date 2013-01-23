package uk.gov.tna.dri.schema

import util.parsing.combinator._

object SchemaParser extends JavaTokenParsers {

  def strToInt(s: String): Int = Integer.parseInt(s, 10)

  def schema = "{" ~> totalColumns ~ country <~ "}" ^^ {case totalColumns ~ country => Schema(totalColumns, country)}

  def positiveNumber = """\d+""".r

  def white = regex("\\s*"r)

  //def totalColumns = "@TotalColumns : " ~> positiveNumber ^^ { _.toInt } - this should work - in scala !
  def totalColumns = "@TotalColumns : " ~> positiveNumber ^^ { strToInt }

  def country = regex("@Country : "r) ~> regex("[a-zA-Z]+"r)
}
