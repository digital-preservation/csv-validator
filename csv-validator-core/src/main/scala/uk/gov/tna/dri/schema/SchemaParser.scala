package uk.gov.tna.dri.schema

import scala.util.parsing.combinator._
import java.io.Reader
import scala.util.Try
import scala._
import collection.mutable
import scala.Some

trait SchemaParser extends RegexParsers {

  override protected val whiteSpace = """[ \t]*""".r

  val white: Parser[String] = whiteSpace

  val eol = sys.props("line.separator")

  val columnIdentifier: Parser[String] = ("""\w+\b"""r) withFailureMessage("Column identifier invalid")

  val positiveNumber: Parser[String] = """[1-9][0-9]*"""r

  val Regex = """([(]")(.*?)("[)])"""r

  val regexParser: Parser[String] = Regex withFailureMessage("""regex not correctly delimited as ("your regex")""")

  def parse(reader: Reader) = parseAll(schema, reader) match {
    case s @ Success(schema: Schema, next) => {
      val messages = valid(schema.globalDirectives, schema.columnDefinitions)
      if (messages.isEmpty) s else Failure(messages, next)
    }

    case n @ NoSuccess(messages, next) => n
  }

  def schema = globalDirectives ~ columnDefinitions ^^ { case g ~ c => Schema(g, c)}

  def globalDirectives: Parser[List[GlobalDirective]] = rep(positioned(globalDirective)) <~ (whiteSpace ~ (eol | endOfInput | failure("Global directives contains invalid text")))

  def globalDirective = totalColumns | noHeaderDirective | ignoreColumnNameCaseDirective

  def totalColumns: Parser[TotalColumns] = (("@totalColumns" ~ white) ~> positiveNumber ^^ { posInt => TotalColumns(posInt.toInt) }).withFailureMessage("@totalColumns invalid")

  def noHeaderDirective: Parser[NoHeader] = "@noHeader" ~ white ^^^ NoHeader()

  def ignoreColumnNameCaseDirective: Parser[IgnoreColumnNameCase] = "@ignoreColumnNameCase" ~ white ^^^ IgnoreColumnNameCase()

  def columnDefinitions = rep1(positioned(columnDefinition))

  def columnDefinition = (columnIdentifier <~ ":") ~ rep(rule) ~ rep(columnDirective) <~ endOfColumnDefinition ^^ {
    case id ~ rules ~ columnDirectives => ColumnDefinition(id, rules, columnDirectives)
  }

  def columnDirective = positioned(optional | ignoreCase)

  def rule = positioned(orRule | unaryRule)

  def unaryRule = regex | fileExistsRule | inRule | isRule | startsRule | uri | xDateTime | xDate | ukDate | xTime | uuid4 | positiveInteger | failure("Invalid rule")

  def orRule: Parser[OrRule] = unaryRule ~ "or" ~ rule  ^^ { case lhs ~ _ ~ rhs => OrRule(lhs, rhs) }

  def regex = "regex" ~> regexParser ^? (validateRegex, s => s"regex invalid: ${s}") | failure("Invalid regex rule")

  def inRule = "in(" ~> argProvider <~ ")" ^^ { InRule  }

  def isRule = "is(" ~> argProvider <~ ")" ^^ { IsRule }

  def startsRule = "starts(" ~> argProvider <~ ")" ^^ { StartsRule }

  def uri: Parser[UriRule] = "uri" ^^^ UriRule()

  def xDateTime: Parser[XsdDateTimeRule] = "xDateTime" ^^^ XsdDateTimeRule()

  def xDate: Parser[XsdDateRule] = "xDate" ^^^ XsdDateRule()

  def ukDate: Parser[UkDateRule] = "ukDate" ^^^ UkDateRule()

  def xTime: Parser[XsdTimeRule] = "xTime" ^^^ XsdTimeRule()

  def uuid4: Parser[Uuid4Rule] = "uuid4" ^^^ Uuid4Rule()

  def positiveInteger: Parser[PositiveIntegerRule] = "positiveInteger" ^^^ PositiveIntegerRule()

  def argProvider: Parser[ArgProvider] = "$" ~> columnIdentifier ^^ { s => ColumnReference(s) } | '\"' ~> """\w+""".r <~ '\"' ^^ {s => Literal(Some(s)) }

  def fileArgProvider: Parser[ArgProvider] = "$" ~> columnIdentifier ^^ { s => ColumnReference(s) } | '\"' ~> rootFilePath <~ '\"' ^^ {s => Literal(Some(s)) }

  def fileExistsRule = ("fileExists(" ~> fileArgProvider <~ ")" ^^ { s => FileExistsRule(s) }).withFailureMessage("Column definition requires a file path") |
    "fileExists" ^^^ { FileExistsRule() } | failure("Invalid fileExists rule")

  def rootFilePath: Parser[String] = """[a-zA-Z/-_\.\d\\:]+""".r

  def optional = "@optional" ^^^ Optional()

  def ignoreCase = "@ignoreCase" ^^^ IgnoreCase()

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

  private def valid(g: List[GlobalDirective], c: List[ColumnDefinition]): String = {
   (totalColumnsValid(g, c).getOrElse("") ::
    columnDirectivesValid(c).getOrElse("") ::
    duplicateColumnsValid(c).getOrElse("") ::
    crossColumnsValid(c).getOrElse("") :: Nil).filter(!_.isEmpty).mkString("\n")
  }

  private def totalColumnsValid(g: List[GlobalDirective], c: List[ColumnDefinition]): Option[String] = {
    val tc: Option[TotalColumns] = g.collectFirst { case t@TotalColumns(_) => t }

    if (!tc.isEmpty && tc.get.numberOfColumns != c.length)
      Some(s"@totalColumns = ${tc.get.numberOfColumns} but number of columns defined = ${c.length} at line: ${tc.get.pos.line}, column: ${tc.get.pos.column}" )
    else
      None
  }

  private def duplicateColumnsValid(columnDefinitions: List[ColumnDefinition]): Option[String] = {
    val groupedColumnDefinitions = columnDefinitions.groupBy(identity)
    val duplicates: Map[ColumnDefinition, List[ColumnDefinition]] = groupedColumnDefinitions.filter( _._2.length > 1)

    // These nasty 2 lines are to keep presentation of error messages in order of column definitions - TODO Refactor on next iteration
    var sortedDuplicates = mutable.LinkedHashMap.empty[ColumnDefinition, List[ColumnDefinition]]
    for (cd <- columnDefinitions; if !sortedDuplicates.contains(cd) && duplicates.contains(cd)) {sortedDuplicates += (cd -> duplicates.get(cd).get)}

    if (sortedDuplicates.isEmpty) None
    else Some(sortedDuplicates.map { case (cd, cds) => s"""Column: ${cd.id} has duplicates on lines """ + cds.map(cd => cd.pos.line).mkString(", ") }.mkString("\n"))
  }

  private def columnDirectivesValid(columnDefinitions: List[ColumnDefinition]): Option[String] = {
    val v = for {
      cd <- columnDefinitions
      if (cd.directives.distinct.length != cd.directives.length)
    } yield {
      s"${cd.id}: Duplicated column directives: " +
        cd.directives.groupBy(identity).filter { case (_, cds) => cds.size > 1}.map { case (cd, _) => "@" + cd + s" at line: ${cd.pos.line}, column: ${cd.pos.column}"}.mkString(",")
    }

    if (v.isEmpty) None else Some(v.mkString("\n"))
  }

  private def crossColumnsValid(columnDefinitions: List[ColumnDefinition]): Option[String] = {
    def filterRules(columnDef:ColumnDefinition ): List[Rule] = { // List of failing rules
      columnDef.rules.filter(rule => {
        rule.argProvider match {
          case ColumnReference(name) => !columnDefinitions.exists(col => col.id == name)
          case _ => false
        }
      })
    }

    def crossReferenceErrors(rules: List[Rule]): String = {
      val errors = rules.map {
        case rule: Rule => s"""${rule.toError} at line: ${rule.pos.line}, column: ${rule.pos.column}"""
        case _ => ""
      }.filter(!_.isEmpty)

      (if (errors.length == 1) "cross reference " else "cross references ") + errors.mkString(", ")
    }

    val errors = columnDefinitions.map(cd => (cd, filterRules(cd))).filter(x => x._2.length > 0)

    if (errors.isEmpty) None
    else Some(errors.map { case (cd, rules) => s"Column: ${cd.id} has invalid ${crossReferenceErrors(rules)}" }.mkString("\n"))
  }
}