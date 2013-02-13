package uk.gov.tna.dri.schema

import util.parsing.combinator._
import java.io.Reader
import util.Try

trait SchemaParser extends RegexParsers {

  override protected val whiteSpace = """[ \t]*""".r

  val white: Parser[String] = whiteSpace

  val eol = sys.props("line.separator")

  val columnIdentifier: Parser[String] = ("""\w+\b"""r) withFailureMessage("Column identifier invalid")

  val positiveNumber: Parser[String] = """[1-9][0-9]*"""r

  val Regex = """([(]")(.*?)("[)])"""r

  val regexParser: Parser[String] = Regex withFailureMessage("""regex not correctly delimited as ("your regex")""")

  def parse(reader: Reader) = parseAll(schema, reader)

  def schema = totalColumns ~ columnDefinitions ^? (createSchema, { case t ~ c => s"Schema invalid as @TotalColumns = ${t} but number of columns defined = ${c.length}" })

  def totalColumns = (("@TotalColumns" ~ white) ~> positiveNumber <~ eol ^^ { _.toInt }).withFailureMessage("@TotalColumns invalid")

  def columnDefinitions = rep1(columnDefinition)

  def columnDefinition = (columnIdentifier <~ ":") ~ rep(columnRules) ~ rep(columnDirectives) <~ endOfColumnDefinition ^^ {
    case id ~ rules ~ columnDirectives => ColumnDefinition(id, rules, columnDirectives)
  }

  def columnRules = regex | inRule | fileExistsRule

  def columnDirectives = optional | ignoreCase

  def regex = "regex" ~> regexParser ^? (validateRegex, s => s"regex invalid: ${s}") | failure("Invalid regex rule")

  def inRule = "in(\"" ~> stringProvider <~ "\")" ^^ { InRule  }

  def stringProvider: Parser[StringProvider] = "$" ~> """\w+""".r ^^ { ColumnTypeProvider } | """\w+""".r ^^ { LiteralTypeProvider }

  def fileExistsRule = "fileExists(\"" ~> rootFilePath <~ "\")" ^^ { s => FileExistsRule(Some(s)) } | "fileExists()" ^^^ { FileExistsRule(None) } | failure("Invalid fileExists rule")

  def rootFilePath: Parser[String] = """[a-zA-Z/-_\.\d\\]+""".r

  def optional = "@Optional" ^^^ Optional()

  def ignoreCase = "@IgnoreCase" ^^^ IgnoreCase()

  private def createSchema: PartialFunction[~[Int, List[ColumnDefinition]], Schema] = {
    case totalColumns ~ columnDefinitions if totalColumns == columnDefinitions.length => Schema(totalColumns, columnDefinitions)
  }

  private def endOfColumnDefinition: Parser[Any] = whiteSpace ~ (eol | endOfInput | failure("Column definition contains invalid text"))

  private def endOfInput: Parser[Any] = new Parser[Any] {
    def apply(input: Input) = {
      if (input.atEnd) new Success("End of Input reached", input)
      else Failure("End of Input expected", input)
    }
  }

  private def validateRegex: PartialFunction[String, RegexRule] = {
    case Regex(_, s, _) if Try(s.r).isSuccess => RegexRule(s.r)
  }
}