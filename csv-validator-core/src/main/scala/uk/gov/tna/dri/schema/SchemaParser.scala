package uk.gov.tna.dri.schema

import util.parsing.combinator._
import java.io.Reader
import util.Try
import util.matching.Regex

trait SchemaParser extends JavaTokenParsers {

  val eol = sys.props("line.separator").head

  def parse(reader: Reader) = parseAll(schema, reader)

  def schema = totalColumns ~ columnDefinitions ^? (createSchema, { case tc ~ c => s"Schema invalid as @TotalColumns = ${tc} but number of columns defined = ${c.length}" })

  def totalColumns = ("@TotalColumns " ~> positiveNumber <~ eol ^^ { _.toInt }).withFailureMessage("@TotalColumns invalid")

  def columnDefinitions = repsep(columnDefinition, eol)

  def columnDefinition = (stringLiteral ~ opt(regex)).withFailureMessage("Column definition invalid") ^^ { case s ~ r => if (r.isEmpty) ColumnDefinition(unquote(s), Nil) else ColumnDefinition(unquote(s), List(RegexRule(r.get))) }

  def regex = ("regex " ~> stringLiteral).withFailureMessage("regex rule invalid") ^? (isValidRegex, s => "regex invalid: " + unquote(s))

  private def createSchema: PartialFunction[~[Int, List[ColumnDefinition]], Schema] = { case totalCols ~ colDefs if totalCols == colDefs.length => Schema(totalCols, colDefs) }

  private def isValidRegex: PartialFunction[String, Regex] = { case s: String if Try(unquote(s).r).isSuccess => unquote(s).r }

  private def positiveNumber = """[1-9][0-9]*""".r

  private def unquote(str: String): String = str.tail.dropRight(1)
}
