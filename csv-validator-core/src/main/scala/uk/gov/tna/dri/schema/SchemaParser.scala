package uk.gov.tna.dri.schema

import scala.util.parsing.combinator._
import java.io.Reader
import scala.util.Try
import collection.immutable.TreeMap
import java.security.MessageDigest
import scalaz._
import Scalaz._

trait SchemaParser extends RegexParsers {

  override protected val whiteSpace = """[ \t]*""".r

  val white: Parser[String] = whiteSpace

  val eol = sys.props("line.separator")

  val columnIdentifier: Parser[String] = """\s*[0-9a-zA-Z_\-.]+""".r withFailureMessage("Column identifier invalid")

  val positiveNumber: Parser[String] = """[1-9][0-9]*""".r

  val stringRegex = """([^"\p{Cntrl}\\]|\\[\\'"bfnrt]|\\u[a-fA-F0-9]{4})*""".r

  val Regex = """([(]")(.*?)("[)])""".r

  val regexParser: Parser[String] = Regex withFailureMessage("""regex not correctly delimited as ("your regex")""")

  def parseAndValidate(reader: Reader): ValidationNEL[String, Schema] = {
    parse(reader) match {
      case s @ Success(schema: Schema, next) => {
        val errors = validate(schema.globalDirectives, schema.columnDefinitions)
        if (errors.isEmpty) schema.successNel[String] else errors.failNel[Schema]
      }
      case n : NoSuccess => n.toString.failNel[Schema]
    }
  }

  def parse(reader: Reader) = parseAll(schema, reader)

  def version: Parser[String] = ("version " ~> Schema.version <~ eol).withFailureMessage(s"version ${Schema.version} missing or incorrect")

  def schema = version ~ globalDirectives ~ columnDefinitions ^^ { case v ~ g ~ c => Schema(g, c)}

  def globalDirectives: Parser[List[GlobalDirective]] = rep(positioned(globalDirective)) <~ (whiteSpace ~ (eol | endOfInput | failure("Global directives contains invalid text")))

  def globalDirective = totalColumns | noHeaderDirective | ignoreColumnNameCaseDirective

  def totalColumns: Parser[TotalColumns] = (("@totalColumns" ~ white) ~> positiveNumber ^^ { posInt => TotalColumns(posInt.toInt) }).withFailureMessage("@totalColumns invalid")

  def noHeaderDirective: Parser[NoHeader] = "@noHeader" ~ white ^^^ NoHeader()

  def ignoreColumnNameCaseDirective: Parser[IgnoreColumnNameCase] = "@ignoreColumnNameCase" ~ white ^^^ IgnoreColumnNameCase()

  def columnDefinitions = rep1(positioned(columnDefinition))

  def columnDefinition = ((columnIdentifier <~ ":") ~ rep(rule) ~ rep(columnDirective) <~ endOfColumnDefinition ^^ {
    case id ~ rules ~ columnDirectives => ColumnDefinition(id, rules, columnDirectives)
  }).withFailureMessage("Column definition contains invalid text")

  def columnDirective = positioned(optional | ignoreCase)

  def rule = positioned(or | unaryRule)

  def unaryRule = regex | fileExists | in | is | isNot | starts | ends | unique | uri | xDateTime | xDate | ukDate | xTime | uuid4 | positiveInteger | checksum | failure("Invalid rule")

  def or: Parser[OrRule] = unaryRule ~ "or" ~ rule  ^^ { case lhs ~ _ ~ rhs => OrRule(lhs, rhs) }

  def regex = "regex" ~> regexParser ^? (validateRegex, s => s"regex invalid: $s") | failure("Invalid regex rule")

  def in = "in(" ~> argProvider <~ ")" ^^ { InRule  }

  def is = "is(" ~> argProvider <~ ")" ^^ { IsRule }

  def isNot = "isNot(" ~> argProvider <~ ")" ^^ { IsNotRule }

  def starts = "starts(" ~> argProvider <~ ")" ^^ { StartsRule }

  def ends = "ends(" ~> argProvider <~ ")" ^^ { EndsRule }

  def unique: Parser[UniqueRule] = "unique" ^^^ UniqueRule()

  def uri: Parser[UriRule] = "uri" ^^^ UriRule()

  def xDateTime: Parser[XsdDateTimeRule] = "xDateTime" ^^^ XsdDateTimeRule()

  def xDate: Parser[XsdDateRule] = "xDate" ^^^ XsdDateRule()

  def ukDate: Parser[UkDateRule] = "ukDate" ^^^ UkDateRule()

  def xTime: Parser[XsdTimeRule] = "xTime" ^^^ XsdTimeRule()

  def uuid4: Parser[Uuid4Rule] = "uuid4" ^^^ Uuid4Rule()

  def positiveInteger: Parser[PositiveIntegerRule] = "positiveInteger" ^^^ PositiveIntegerRule()

  def argProvider: Parser[ArgProvider] = "$" ~> columnIdentifier ^^ { s => ColumnReference(s) } | '\"' ~> stringRegex <~ '\"' ^^ {s => Literal(Some(s)) }

  def fileArgProvider: Parser[ArgProvider] = "$" ~> columnIdentifier ^^ { s => ColumnReference(s) } | '\"' ~> rootFilePath <~ '\"' ^^ {s => Literal(Some(s)) }

  def fileExists = ("fileExists(" ~> fileArgProvider <~ ")" ^^ { s => FileExistsRule(s) }).withFailureMessage("fileExists rule has an invalid file path") |
    "fileExists" ^^^ { FileExistsRule() } | failure("Invalid fileExists rule")

  def rootFilePath: Parser[String] = """[a-zA-Z/-_\.\d\\:]+""".r

  def checksum = "checksum(" ~> fileExpr ~ ("," ~ white) ~ algorithm <~ ")" ^^ { case a ~ _ ~ algo => ChecksumRule(a._1.getOrElse(Literal(None)), a._2, algo) }

  def fileExpr = "file(" ~> opt(argProvider <~ ("," ~ white)) ~ argProvider <~ ")" ^^ { a => a }

  def algorithm: Parser[String] = "\"" ~> stringRegex <~ "\"" ^? (validateAlgorithm, s => "Invalid Algorithm " + s)

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

  private def validateAlgorithm: PartialFunction[String, String] = {
    case s: String if Try(MessageDigest.getInstance(s)).isSuccess => s
  }

  private def validate(g: List[GlobalDirective], c: List[ColumnDefinition]): String =
    totalColumnsValid(g, c) :: columnDirectivesValid(c) :: duplicateColumnsValid(c) :: crossColumnsValid(c) :: Nil collect  { case Some(s: String) => s } mkString("\n")

  private def totalColumnsValid(g: List[GlobalDirective], c: List[ColumnDefinition]): Option[String] = {
    val tc: Option[TotalColumns] = g.collectFirst { case t @ TotalColumns(_) => t }

    if (!tc.isEmpty && tc.get.numberOfColumns != c.length)
      Some(s"@totalColumns = ${tc.get.numberOfColumns} but number of columns defined = ${c.length} at line: ${tc.get.pos.line}, column: ${tc.get.pos.column}" )
    else
      None
  }

  private def duplicateColumnsValid(columnDefinitions: List[ColumnDefinition]): Option[String] = {
    val duplicates = TreeMap(columnDefinitions.groupBy(_.id).toSeq:_*).filter(_._2.length > 1)

    if (duplicates.isEmpty) None
    else Some(duplicates.map { case (id, cds) => s"""Column: $id has duplicates on lines """ + cds.map(cd => cd.pos.line).mkString(", ") }.mkString("\n"))
  }

  private def columnDirectivesValid(columnDefinitions: List[ColumnDefinition]): Option[String] = {
    val v = for {
      cd <- columnDefinitions
      if (cd.directives.distinct.length != cd.directives.length)
    } yield {
      s"${cd.id}: Duplicated column directives: " +
      cd.directives.groupBy(identity).filter { case (_, cds) => cds.size > 1}.map { case (cdId, _) => "@" + cdId + s" at line: ${cdId.pos.line}, column: ${cdId.pos.column}"}.mkString(", ")
    }

    if (v.isEmpty) None else Some(v.mkString("\n"))
  }

  private def crossColumnsValid(columnDefinitions: List[ColumnDefinition]): Option[String] = {
    def filterRules(cds: ColumnDefinition ): List[Rule] = { // List of failing rules
      cds.rules.filter(rule => {
        def undefinedCross(a: ArgProvider) = a match {
          case ColumnReference(name) => !columnDefinitions.exists(col => col.id == name)
          case _ => false
        }

        rule.argProviders.foldLeft(false)((acc, arg) => acc || undefinedCross(arg))
      })
    }

    def crossReferenceErrors(rules: List[Rule]): String = {
      val errors = rules collect { case rule: Rule => s"""${rule.toError} at line: ${rule.pos.line}, column: ${rule.pos.column}""" }

      (if (errors.length == 1) "cross reference " else "cross references ") + errors.mkString(", ")
    }

    val errors = columnDefinitions.map(cd => (cd, filterRules(cd))).filter(_._2.length > 0)

    if (errors.isEmpty) None
    else Some(errors.map { case (cd, rules) => s"Column: ${cd.id} has invalid ${crossReferenceErrors(rules)}" }.mkString("\n"))
  }
}