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

  def columnDefinition = (columnIdentifier <~ ":") ~ rep(rules) ~ rep(columnDirectives) <~ endOfColumnDefinition ^^ {
    case id ~ rules ~ columnDirectives => ColumnDefinition(id, rules, columnDirectives)
  }

  def rules = regex | inRule| fileExistsRule

  def columnDirectives = optional | ignoreCase

  def regex = "regex" ~> regexParser ^? (validateRegex, s => s"regex invalid: ${s}") | failure("Invalid regex rule")

  def inRule = "in(" ~> argProvider <~ ")" ^^ { InRule  }

  def argProvider: Parser[ArgProvider] = "$" ~> """\w+""".r ^^ { s => ColumnReference(s) } | '\"' ~> """\w+""".r <~ '\"' ^^ {s => Literal(Some(s)) }

  def fileExistsRule = "fileExists(\"" ~> rootFilePath <~ "\")" ^^ { s => FileExistsRule(Literal(Some(s))) } |
                       "fileExists" ^^^ { FileExistsRule(Literal(None)) } | failure("Invalid fileExists rule")

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
    case Regex(_, s, _) if Try(s.r).isSuccess => RegexRule(Literal(Some(s)))
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
      case InRule(s) => findColumnName(s)
      case _ => None
    }

    def findColumnName(s: ArgProvider): Option[String] = s match {
      case ColumnReference(name) => Some(name)
      case _ => None
    }

    def crossReferenceErrors(rules: List[Rule]): String = rules.map {
      case rule: InRule => s""" ${rule.name}: ${rule.inValue.argVal.getOrElse("")}"""
      case _ => ""
    }.mkString(",")

    val errors = columnDefinitions.map(columnDef => (columnDef, filterRules(columnDef))).filter(x => x._2.length > 0)

    if (errors.isEmpty) {
      None
    } else {
      val errorMessages = errors.map(e => s"Column: ${e._1.id} has invalid cross reference${crossReferenceErrors(e._2)}")
      Some(errorMessages.mkString("\n"))
    }
  }
}