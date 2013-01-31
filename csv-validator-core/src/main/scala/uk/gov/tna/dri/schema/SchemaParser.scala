package uk.gov.tna.dri.schema

import util.parsing.combinator._
import java.io.Reader
import util.Try
import util.matching.Regex

trait SchemaParser extends JavaTokenParsers {

  def parse(reader: Reader) = parseAll(schema, reader)

  def schema = totalColumns ~ rep(column) ^^ { case totalColumns ~ columns => Schema(totalColumns, columns) }

  def totalColumns = "@TotalColumns " ~> positiveNumber ^^ { _.toInt } | failure("@TotalColumns invalid")

  def column = stringLiteral ~ opt(regex) ^^ { case s ~ r => if (r.isEmpty) ColumnDefinition(unquote(s), Nil) else ColumnDefinition(unquote(s), List(RegexRule(r.get))) }

  def regex = "regex " ~> stringLiteral ^? (isValidRegex, s => "regex invalid: " + unquote(s)) | failure("Invalid regex rule")

  private def isValidRegex: PartialFunction[String, Regex] = { case s: String if Try(unquote(s).r).isSuccess => unquote(s).r }

  private def positiveNumber = """[1-9][0-9]*""".r

  private def unquote(str: String): String = str.tail.dropRight(1)
}
