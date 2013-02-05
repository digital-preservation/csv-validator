package uk.gov.tna.dri.schema

import util.parsing.combinator._
import java.io.Reader
import util.Try

trait SchemaParser extends RegexParsers {

  override val skipWhitespace = false

  override protected val whiteSpace = """[ \t]*""".r

  val eol = sys.props("line.separator")

  val white: Parser[String] = whiteSpace

  val columnIdentifier: Parser[String] = ("""\w+\b"""r) withFailureMessage("Column identifier invalid")

  val positiveNumber: Parser[String] = """[1-9][0-9]*"""r

  val quote: Parser[String] = "\""r

  val quotedRegex: Parser[String] = "\".*?\""r

  def parse(reader: Reader) = parseAll(schema, reader)

  def schema = totalColumns ~ columnDefinitions ^? (createSchema, { case tc ~ c => s"Schema invalid as @TotalColumns = ${tc} but number of columns defined = ${c.length}" })

  def totalColumns = (("@TotalColumns" ~ white) ~> positiveNumber <~ eol ^^ { _.toInt }).withFailureMessage("@TotalColumns invalid")

  def columnDefinitions = rep1(columnDefinition)

  def columnDefinition = (white ~> columnIdentifier <~ (white ~ ":" ~ white)) ~ opt(regex) <~ endOfColumnDefinition ^^ {
    case i ~ r => ColumnDefinition(i, List(r).collect { case Some(r) => r })
  }

  def regex = ("regex" ~ white) ~> (quotedRegex withFailureMessage("regex definition missing quotes")) ^? (isValidRegex, s => "regex invalid: " + s) | failure("Invalid regex rule")

  private def createSchema: PartialFunction[~[Int, List[ColumnDefinition]], Schema] = {
    case totalColumns ~ columnDefinitions if totalColumns == columnDefinitions.length => Schema(totalColumns, columnDefinitions)
  }

  private def endOfColumnDefinition: Parser[Any] = white ~ (eol | endOfInput | failure("Column definition contains invalid (extra) text"))

  private def endOfInput: Parser[Any] = new Parser[Any] {
    def apply(input: Input) = {
      if (input.atEnd) new Success("End of Input reached", input)
      else Failure("End of Input expected", input)
    }
  }

  private def isValidRegex: PartialFunction[String, RegexRule] = { case s: String if Try(unquote(s).r).isSuccess => RegexRule(unquote(s).r) }

  private def unquote(str: String): String = str.tail.dropRight(1)
}
