package uk.gov.tna.dri.schema

import util.parsing.combinator._
import java.io.Reader

trait SchemaParser extends JavaTokenParsers {

  def parse(reader: Reader) = parseAll(schema, reader)

  def schema = totalColumns ^^ { Schema(_) }

  def totalColumns = "@TotalColumns " ~> positiveNumber ^^ { _.toInt }

  private def positiveNumber = """[1-9][0-9]*""".r
}
