package uk.gov.tna.dri.schema

import util.parsing.combinator.JavaTokenParsers

object SchemaParser extends JavaTokenParsers {

  def parse(schema: String) = {
    parseAll(rules, schema) match {
      case Success(result, _) => Schema(result)
      case Failure(message, input) => message + " at line: " + input.pos.line + ", column: " + input.pos.column
    }
  }

  def rules: Parser[String] = "@TotalColumns : " ~> positiveNumber

  def positiveNumber: Parser[String] = """\d+""".r
}
