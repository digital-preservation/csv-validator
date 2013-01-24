package uk.gov.tna.dri.schema

import util.parsing.combinator._

trait SchemaParser extends JavaTokenParsers {

  def parse(schemaText: String) = parseAll(schemaGrammer, schemaText)

  def schemaGrammer = "{" ~> totalColumns ~ opt(quoted) <~ "}" ^^ { case totalColumns ~ quoted => Schema(totalColumns, quoted) }

  //def totalColumns = "@TotalColumns : " ~> positiveNumber ^^ { _.toInt } - this should work - in scala !
  def totalColumns = "@TotalColumns " ~> positiveNumber ^^ { strToInt }

  def quoted = "@Quoted " ~> regex("-"r)

  private def strToInt(s: String): Int = Integer.parseInt(s, 10)

  private def positiveNumber = """[1-9][0-9]*""".r

  private def white = regex("\\s*"r)
}
