package uk.gov.tna.dri.schema

import util.parsing.combinator._
import java.io.Reader
import util.matching.Regex

trait SchemaParser extends JavaTokenParsers {

  def parse(reader: Reader) = parseAll(schema, reader)

  def schema = totalColumns ~ opt(regex) ^^ { case totalColumns ~ regex => Schema(totalColumns, regex) }

  def totalColumns = "@TotalColumns " ~> positiveNumber ^^ { _.toInt } | failure("@TotalColumns invalid")

  def regex = "regex " ~> stringLiteral ^^ {_.r} | failure("regex invalid")

  private def positiveNumber = """[1-9][0-9]*""".r
}
