package uk.gov.tna.dri.schema

import util.parsing.combinator._
import java.io.Reader
import util.Try
import util.matching.Regex

trait SchemaParser extends JavaTokenParsers {

  def parse(reader: Reader) = parseAll(schema, reader)

  def schema = totalColumns ~ opt(regex) ^^ { case totalColumns ~ regex => Schema(totalColumns, regex) }

  def totalColumns = "@TotalColumns " ~> positiveNumber ^^ { _.toInt } | failure("@TotalColumns invalid")

  def regex = "regex " ~> stringLiteral ^? (isValidRegex, s => "regex invalid: " + unQuote(s))

  def isValidRegex: PartialFunction[String, Regex] = { case s: String if Try(unQuote(s).r).isSuccess => unQuote(s).r }

  private def positiveNumber = """[1-9][0-9]*""".r

  private def unQuote(str: String): String = str.tail.dropRight(1)
}
