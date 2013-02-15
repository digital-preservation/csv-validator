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

  def schema = totalColumns ~ columnDefinitions ^? (createSchema, {
    case t ~ c if t != c.length => {
      val cross = crossCheck(c)
      s"@TotalColumns = ${t} but number of columns defined = ${c.length}" + (if (cross.isEmpty) "" else "\n") + cross.getOrElse("")
    }
    case t ~ c => crossCheck(c).getOrElse("")
  })

  def totalColumns = (("@TotalColumns" ~ white) ~> positiveNumber <~ eol ^^ { _.toInt }).withFailureMessage("@TotalColumns invalid")

  def columnDefinitions = rep1(columnDefinition)

  def columnDefinition = (columnIdentifier <~ ":") ~ rep(columnRules) ~ rep(columnDirectives) <~ endOfColumnDefinition ^^ {
    case id ~ rules ~ columnDirectives => ColumnDefinition(id, rules, columnDirectives)
  }

  def columnRules = regex | isRule | notRule | inRule | startsRule | endsRule | fileExistsRule

  def columnDirectives = optional | ignoreCase

  def regex = "regex" ~> regexParser ^? (validateRegex, s => s"regex invalid: ${s}") | failure("Invalid regex rule")

  def isRule = "is(" ~> stringProvider <~ ")" ^^ { IsRule  }

  def notRule = "not(" ~> stringProvider <~ ")" ^^ { NotRule  }

  def inRule = "in(" ~> stringProvider <~ ")" ^^ { InRule  }

  def startsRule = "starts(" ~> stringProvider <~ ")" ^^ { StartsRule  }

  def endsRule = "ends(" ~> stringProvider <~ ")" ^^ { EndsRule  }

  def stringProvider: Parser[StringProvider] = "$" ~> """\w+""".r ^^ { ColumnTypeProvider } | '\"' ~> """\w+""".r <~ '\"' ^^ { LiteralTypeProvider }

  def fileExistsRule = "fileExists(\"" ~> rootFilePath <~ "\")" ^^ { s => FileExistsRule(Some(s)) } | "fileExists" ^^^ { FileExistsRule(None) } | failure("Invalid fileExists rule")

  def rootFilePath: Parser[String] = """[a-zA-Z/-_\.\d\\]+""".r

  def optional = "@Optional" ^^^ Optional()

  def ignoreCase = "@IgnoreCase" ^^^ IgnoreCase()

  private def createSchema: PartialFunction[~[Int, List[ColumnDefinition]], Schema] = {
    case totalColumns ~ columnDefinitions if totalColumns == columnDefinitions.length && crossCheck(columnDefinitions).isEmpty => Schema(totalColumns, columnDefinitions)
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

  private def crossCheck(columnDefinitions: List[ColumnDefinition]): Option[String] = {

    def filterRules(columnDef:ColumnDefinition ): List[Rule] = { // List of failing rules
      columnDef.rules.filter(rule => {
        findColumnReference(rule) match {
          case Some(name) => !columnDefinitions.exists(col => col.id == name)
          case None => false
        }
      })
    }

    def findColumnReference(rule: Rule): Option[String] = rule match {
      case IsRule(s) => findColumnName(s)
      case NotRule(s) => findColumnName(s)
      case InRule(s) => findColumnName(s)
      case StartsRule(s) => findColumnName(s)
      case EndsRule(s) => findColumnName(s)
      case _ => None
    }

    def findColumnName(s: StringProvider): Option[String] = s match {
      case ColumnTypeProvider(name) => Some(name)
      case _ => None
    }

    def prettyInRules(inRule: List[Rule]): String = inRule.map {
      case rule: IsRule => s" ${rule.name}: ${rule.inVal.value}"
      case rule: NotRule => s" ${rule.name}: ${rule.inVal.value}"
      case rule: InRule => s" ${rule.name}: ${rule.inVal.value}"
      case rule: StartsRule => s" ${rule.name}: ${rule.inVal.value}"
      case rule: EndsRule => s" ${rule.name}: ${rule.inVal.value}"
      case _ => ""
    }.mkString(",")

    val errors = columnDefinitions.map(columnDef => (columnDef, filterRules(columnDef))).filter(x => x._2.length > 0)
    lazy val errorMessages = errors.map(e => s"Column: ${e._1.id} has invalid cross reference${prettyInRules(e._2)}")
    if (errors.isEmpty) None else Some(errorMessages.mkString("\n"))
  }
}