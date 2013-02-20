package uk.gov.tna.dri.schema

import scala.util.parsing.combinator._
import java.io.Reader
import scala.util.Try
import scala._
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

  def columnDefinitions = rep1(columnDefinition)

  def columnDefinition = (columnIdentifier <~ ":") ~ rep(rule) ~ rep(columnDirective) <~ endOfColumnDefinition ^^ {
    case id ~ rules ~ columnDirectives => ColumnDefinition(id, rules, columnDirectives)
  }

  def rule = positioned(orRule | unaryRule)

  def unaryRule = regex | inRule | fileExistsRule | failure("Invalid rule")

  def orRule: Parser[OrRule] = unaryRule ~ "or" ~ rule  ^^ { case lhs ~ _ ~ rhs => OrRule(lhs, rhs) }

  def columnDirective = positioned(optional | ignoreCase)

  def regex = "regex" ~> regexParser ^? (validateRegex, s => s"regex invalid: ${s}") | failure("Invalid regex rule")

  def inRule = "in(" ~> argProvider <~ ")" ^^ { InRule  }

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
    val tc: Option[TotalColumns] = g.collectFirst { case t@TotalColumns(_) => t }

    val columnDirectives = columnDirectivesValid(c)
    val cross = crossColumnsValid(c)

    if (!tc.isEmpty && tc.get.numberOfColumns != c.length) {
      s"@totalColumns = ${tc.get.numberOfColumns} but number of columns defined = ${c.length} at line: ${tc.get.pos.line}, column: ${tc.get.pos.column}" +
                        (if (cross.isEmpty) "" else "\n") + cross.getOrElse("")
    } else {
      columnDirectives.getOrElse("") + cross.getOrElse("")
    }
  }

  private def duplicateColumnsValid(col: List[ColumnDefinition]): Map[ColumnDefinition, List[Int]] = {
    val columnsByColumnId = col.zipWithIndex.groupBy { _._1 }
    columnsByColumnId.filter( _._2.length > 1 ).map { case (id,idAndPos) => (id, idAndPos.map{ case (id, pos) => pos}) }
  }

  private def columnDirectivesValid(col: List[ColumnDefinition]): Option[String] = {
    val v = for {
      colDef <- col
      if (colDef.directives.distinct.length != colDef.directives.length)
    } yield {
      s"${colDef.id}: Duplicated column directives: " +
       colDef.directives.groupBy(identity).filter(p => p._2.size > 1).map(p => "@" + p._1 + s" at line: ${p._1.pos.line}, column: ${p._1.pos.column}").mkString(",")
    }

    if (v.isEmpty) None else Some(v.mkString("\n"))
  }

  private def crossColumnsValid(columnDefinitions: List[ColumnDefinition]): Option[String] = {

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

    def crossReferenceErrors(rules: List[Rule]): String = {
      val errors = rules.map {
        case rule: InRule => s"""${rule.toError} at line: ${rule.pos.line}, column: ${rule.pos.column}"""
        case _ => ""
      }.filter(!_.isEmpty)

      (if (errors.length == 1) "cross reference " else "cross references ") + errors.mkString(", ")
    }

    val errors = columnDefinitions.map(columnDef => (columnDef, filterRules(columnDef))).filter(x => x._2.length > 0)

    if (errors.isEmpty) {
      None
    } else {
      val errorMessages = errors.map(e => s"Column: ${e._1.id} has invalid ${crossReferenceErrors(e._2)}")
      Some(errorMessages.mkString("\n"))
    }
  }
}