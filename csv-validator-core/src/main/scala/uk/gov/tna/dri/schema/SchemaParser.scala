package uk.gov.tna.dri.schema

import util.parsing.combinator._

object SchemaParser extends JavaTokenParsers {

  def strToInt(s: String): Int = Integer.parseInt(s, 10)

  def schema = "{" ~> totalColumns ~ opt(quoted) <~ "}" ^^ {case totalColumns ~ quoted => Schema(totalColumns, quoted)}

  def positiveNumber = """\d+""".r

  def white = regex("\\s*"r)

  //def totalColumns = "@TotalColumns : " ~> positiveNumber ^^ { _.toInt } - this should work - in scala !
  def totalColumns = "@TotalColumns : " ~> positiveNumber ^^ { strToInt }

  def quoted = regex("@Quoted : "r) ~> regex("-"r)
}
