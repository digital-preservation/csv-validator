package uk.gov.tna.dri.schema

import scala.util.parsing.combinator._
import java.io.Reader
import scala.util.Try
import collection.immutable.TreeMap
import java.security.MessageDigest
import scalaz._
import Scalaz._
import uk.gov.tna.dri.validator.{SchemaMessage, FailMessage}

trait SchemaParser extends RegexParsers {

  override protected val whiteSpace = """[ \t]*""".r

  val white: Parser[String] = whiteSpace

  val eol = """\r?\n""".r

  val columnIdentifier: Parser[String] = """\s*[0-9a-zA-Z_\-.]+""".r withFailureMessage("Column identifier invalid")

  val positiveNumber: Parser[String] = """[1-9][0-9]*""".r

  val nonNegativeNumber: Parser[String] = """[0-9]+""".r

  val number: Parser[BigDecimal] = """(-|\+)*[0-9]*+(\.[0-9]*)?""".r ^^ { BigDecimal(_) }

  val stringRegex = """([^"\p{Cntrl}\\]|\\[\\'"bfnrt]|\\u[a-fA-F0-9]{4})*""".r

  val Regex = """([(]")(.*?)("[)])""".r

  val regexParser: Parser[String] = Regex withFailureMessage("""regex not correctly delimited as ("your regex")""")

  val pathSubstitutions: List[(String,String)]

  def parseAndValidate(reader: Reader): ValidationNEL[FailMessage, Schema] = {
    parse(reader) match {
      case s @ Success(schema: Schema, next) => {
        val errors = validate(schema.globalDirectives, schema.columnDefinitions)
        if (errors.isEmpty) schema.successNel[FailMessage] else SchemaMessage(errors).failNel[Schema]
      }
      case n : NoSuccess => SchemaMessage(n.toString).failNel[Schema]
    }
  }

  def parse(reader: Reader) = parseAll(schema, reader)

  def version: Parser[String] = ("version " ~> Schema.version <~ eol).withFailureMessage(s"version ${Schema.version} missing or incorrect")

  def schema = version ~ globalDirectives ~ rep1((rep(comment) ~> columnDefinitions) <~ rep(comment)) ^^ { case v ~ g ~ c => Schema(g, c)}

  def globalDirectives: Parser[List[GlobalDirective]] = rep(positioned(globalDirective <~ (whiteSpace ~ opt(eol | endOfInput))))

  def globalDirective = totalColumns | noHeaderDirective | ignoreColumnNameCaseDirective

  def totalColumns: Parser[TotalColumns] = (("@totalColumns" ~ white) ~> positiveNumber ^^ { posInt => TotalColumns(posInt.toInt) }).withFailureMessage("@totalColumns invalid")

  def noHeaderDirective: Parser[NoHeader] = "@noHeader" ~ white ^^^ NoHeader()

  def ignoreColumnNameCaseDirective: Parser[IgnoreColumnNameCase] = "@ignoreColumnNameCase" ~ white ^^^ IgnoreColumnNameCase()

  def columnDefinitions = (positioned(columnDefinition))

  def columnDefinition = ((columnIdentifier <~ ":") ~ rep(rule) ~ rep(columnDirective) <~ (endOfColumnDefinition | comment) ^^ {
    case id ~ rules ~ columnDirectives => ColumnDefinition(id, rules, columnDirectives)
  }).withFailureMessage("Invalid schema text")

  def comment: Parser[Any] = singleLineComment | multiLineComment

  def singleLineComment: Parser[String] = """//.*\r?\n""".r

  def multiLineComment: Parser[String] = """((?:/\*(?:[^*]|(?:\*+[^*/]))*\*+/)|(?://.*))\r?\n""".r

  def columnDirective = positioned(optional | ignoreCase | warning)

  def rule = positioned( and | or | nonConditionalRule | conditionalRule)

  def nonConditionalRule = unaryRule

  def conditionalRule = ifExpr

  def unaryRule = regex | fileExists | in | is | isNot | starts | ends  | uniqueMultiExpr | uniqueExpr | uri | xDateTimeRange | xDateTime | xDateRange | xDate | ukDateRange | ukDate | xTimeRange | xTime |
    uuid4 | positiveInteger | checksum | fileCount | parenthesesRule | range | lengthExpr | failure("Invalid rule")

  def parenthesesRule: Parser[ParenthesesRule] = "(" ~> rep1(rule) <~ ")" ^^ { ParenthesesRule(_) } | failure("unmatched paren")

  def or: Parser[OrRule] = unaryRule ~ "or" ~ rule  ^^ { case lhs ~ _ ~ rhs => OrRule(lhs, rhs) }

  def and: Parser[AndRule] = unaryRule ~ "and" ~ rule  ^^  { case lhs ~ _ ~ rhs =>  AndRule(lhs, rhs) }

  def ifExpr: Parser[IfRule] = (("if(" ~> white ~> nonConditionalRule <~ white <~ "," <~ white) ~ (rep1(rule)) ~ opt((white ~> "," ~> white ~> rep1(rule))) <~ white <~ ")" ^^ {
    case cond ~ bdy ~ optBdy => IfRule(cond, bdy, optBdy)
  }) | failure("Invalid rule")

  def regex = "regex" ~> regexParser ^^ { s => RegexRule(s.dropRight(2).drop(2)) }

  def in = "in(" ~> argProvider <~ ")" ^^ { InRule  }

  def is = "is(" ~> argProvider <~ ")" ^^ { IsRule }

  def isNot = "isNot(" ~> argProvider <~ ")" ^^ { IsNotRule }

  def starts = "starts(" ~> argProvider <~ ")" ^^ { StartsRule }

  def ends = "ends(" ~> argProvider <~ ")" ^^ { EndsRule }

  def uniqueExpr: Parser[UniqueRule] = "unique" ^^^ UniqueRule()

  def uniqueMultiExpr: Parser[UniqueMultiRule] = "unique(" ~ white ~ "$" ~> columnIdentifier ~ rep( white ~ ',' ~ "$" ~> columnIdentifier ) <~ ")" ^^ { s => UniqueMultiRule( s._1 :: s._2 ) }

  def uri: Parser[UriRule] = "uri" ^^^ UriRule()

  def xDateTime: Parser[XsdDateTimeRule] = "xDateTime" ^^^ XsdDateTimeRule()

  def xDateTimeExpr: Parser[String] = """[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}""".r

  def xDateTimeRange: Parser[XsdDateTimeRangeRule] = (("xDateTime(" ~> white) ~> xDateTimeExpr <~ (white <~ "," <~ white)) ~ xDateTimeExpr <~ (white ~ ")") ^^  {
    case from ~ to => XsdDateTimeRangeRule(from, to)
  }

  def xDate: Parser[XsdDateRule] = "xDate" ^^^ XsdDateRule()

  def xsdDateExpr: Parser[String] = "[0-9]{4}-[0-9]{2}-[0-9]{2}".r

  def xDateRange: Parser[XsdDateRangeRule] = (("xDate(" ~> white) ~> xsdDateExpr <~ (white <~ "," <~ white)) ~ xsdDateExpr <~ (white ~ ")") ^^  {
    case from ~ to => XsdDateRangeRule(from, to)
  }

  def ukDate: Parser[UkDateRule] = "ukDate" ^^^ UkDateRule()

  val ukDateExpr: Parser[String] = "[0-9]{1,2}/[0-9]{1,2}/[0-9]{4}".r

  def ukDateRange: Parser[UkDateRangeRule] = (("ukDate(" ~> white) ~> ukDateExpr <~ (white <~ "," <~ white)) ~ ukDateExpr <~ (white ~ ")") ^^  {
    case from ~ to => UkDateRangeRule(from, to)
  }

  def xTime: Parser[XsdTimeRule] = "xTime" ^^^ XsdTimeRule()

  val xsdTimeExpr: Parser[String] = "[0-9]{2}:[0-9]{2}:[0-9]{2}".r

  def xTimeRange: Parser[XsdTimeRangeRule] = (("xTime(" ~> white) ~> xsdTimeExpr <~ (white <~ "," <~ white)) ~ xsdTimeExpr <~ (white ~ ")") ^^  {
    case from ~ to => XsdTimeRangeRule(from, to)
  }

  def uuid4: Parser[Uuid4Rule] = "uuid4" ^^^ Uuid4Rule()

  def positiveInteger: Parser[PositiveIntegerRule] = "positiveInteger" ^^^ PositiveIntegerRule()

  def argProvider: Parser[ArgProvider] = "$" ~> columnIdentifier ^^ { s => ColumnReference(s) } | '\"' ~> stringRegex <~ '\"' ^^ {s => Literal(Some(s)) }

  def fileArgProvider: Parser[ArgProvider] = "$" ~> columnIdentifier ^^ { s => ColumnReference(s) } | '\"' ~> rootFilePath <~ '\"' ^^ {s => Literal(Some(s)) }

  def fileExists = ("fileExists(" ~> fileArgProvider <~ ")" ^^ { s => FileExistsRule(pathSubstitutions, s) }).withFailureMessage("fileExists rule has an invalid file path") |
    "fileExists" ^^^ { FileExistsRule( pathSubstitutions ) } | failure("Invalid fileExists rule")

  def rootFilePath: Parser[String] = """[a-zA-Z/-_\.\d\\:]+""".r

  def checksum = "checksum(" ~> file ~ (white ~ "," ~ white) ~ algorithmExpr <~ ")" ^^ { case files ~ _ ~ algorithm => ChecksumRule(files._1.getOrElse(Literal(None)), files._2, algorithm, pathSubstitutions) }

  def fileCount = "fileCount(" ~> file <~ ")" ^^ { case a  => FileCountRule(a._1.getOrElse(Literal(None)), a._2, pathSubstitutions) }

  def range = "range(" ~> number ~ "," ~ number <~ ")"  ^^ { case a ~ _ ~ b =>  RangeRule(a, b) }

  def lengthExpr: Parser[LengthRule] = ("length(" ~> opt(("*"| nonNegativeNumber) <~ ",") ~ ("*" | nonNegativeNumber) <~ ")") ^^ {
    case a ~ b => LengthRule(a,b)
  }

  def dateRange = "dateRange(\""

  def file = "file(" ~> opt(argProvider <~ (white ~ "," ~ white)) ~ argProvider <~ ")" ^^ { a => a }

  def algorithmExpr: Parser[String] = "\"" ~> stringRegex <~ "\""  ^^ { a => a }

  def optional = "@optional" ^^^ Optional()

  def warning = "@warning" ^^^ Warning()

  def ignoreCase = "@ignoreCase" ^^^ IgnoreCase()

  private def endOfColumnDefinition: Parser[Any] = whiteSpace ~ (eol | endOfInput | failure("Invalid schema text"))

  private def endOfInput: Parser[Any] = new Parser[Any] {
    def apply(input: Input) = {
      if (input.atEnd) new Success("End of Input reached", input)
      else Failure("End of Input expected", input)
    }
  }

  private def validate(g: List[GlobalDirective], c: List[ColumnDefinition]): String = {
    globDirectivesValid(g) ::totalColumnsValid(g, c) :: columnDirectivesValid(c) :: duplicateColumnsValid(c) :: crossColumnsValid(c) :: checksumAlgorithmValid(c) ::
    rangeValid(c) :: lengthValid(c) :: regexValid(c) :: dateRangeValid(c) :: uniqueMultiValid(c) :: Nil collect { case Some(s: String) => s } mkString("\n")
  }

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

  private def globDirectivesValid(directives: List[GlobalDirective]): Option[String] = {
    val duplicates = for ((name, lst) <- directives.groupBy(_.name) if (lst.length > 1)) yield {
      s"Global directive @$name is duplicated"
    }

    if (duplicates.isEmpty) None else Some(duplicates.mkString("\n"))
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

  private def checksumAlgorithmValid(columnDefinitions: List[ColumnDefinition]): Option[String] = {

    def algorithmCheck(rule: Rule): Boolean = rule match {
      case checksum:ChecksumRule => Try(
        // 3rd party checksum algorithm may need to be checked here
        // if not added a part of the Service Provider Interface (SPI)
        MessageDigest.getInstance(checksum.algorithm)
      ).isFailure
      case _ => false
    }

    val v = for {
      cd <- columnDefinitions
      rule <- cd.rules
      if (algorithmCheck(rule))
    } yield {
      rule match {
        case checksum:ChecksumRule => s"""Column: ${cd.id}: Invalid Algorithm: '${checksum.algorithm}' at line: ${rule.pos.line}, column: ${rule.pos.column}"""
        case _ =>  s"""Column: ${cd.id}: Invalid Algorithm: at line: ${rule.pos.line}, column: ${rule.pos.column}"""
      }
    }

    if (v.isEmpty) None else Some(v.mkString("\n"))
  }

  private def rangeValid(columnDefinitions: List[ColumnDefinition]): Option[String] = {
    def rangeCheck(rule: Rule): Boolean = rule match {
      case RangeRule(min,max) => min > max
      case _ => false
    }

    val v = for {
      cd <- columnDefinitions
      rule <- cd.rules
      if (rangeCheck(rule))
    } yield {
      rule match {
        case range:RangeRule => s"""Column: ${cd.id}: Invalid range, minimum greater than maximum in: 'range(${range.min},${range.max})' at line: ${rule.pos.line}, column: ${rule.pos.column}"""
        case _ =>  s"""Column: ${cd.id}: Invalid range, minimum greater than maximum: at line: ${rule.pos.line}, column: ${rule.pos.column}"""
      }
    }

    if (v.isEmpty) None else Some(v.mkString("\n"))
  }

  private def lengthValid(columnDefinitions: List[ColumnDefinition]): Option[String] = {
    def lengthCheck(rule: Rule): Boolean = rule match {
      case LengthRule(Some(from),to) => if (from == "*" || to == "*") true else from.toInt <= to.toInt
      case _ => true
    }

    val v = for {
      cd <- columnDefinitions
      rule <- cd.rules
      if (!lengthCheck(rule))
    } yield {
      rule match {
        case len:LengthRule => s"""Column: ${cd.id}: Invalid length, minimum greater than maximum in: 'length(${len.from.getOrElse("")},${len.to})' at line: ${rule.pos.line}, column: ${rule.pos.column}"""
        case _ =>  s"""Column: ${cd.id}: Invalid length, minimum greater than maximum: at line: ${rule.pos.line}, column: ${rule.pos.column}"""
      }
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

  private def regexValid(columnDefinitions: List[ColumnDefinition]): Option[String] = {
    def regexCheck(rule: Rule): Boolean = rule match {
      case RegexRule(s) =>  Try(s.r).isFailure
      case _ => false
    }

    val v = for {
      cd <- columnDefinitions
      rule <- cd.rules
      if (regexCheck(rule))
    } yield s"""Column: ${cd.id}: Invalid ${rule.toError}: at line: ${rule.pos.line}, column: ${rule.pos.column}"""

    if (v.isEmpty) None else Some(v.mkString("\n"))
  }

  private def dateRangeValid(columnDefinitions: List[ColumnDefinition]): Option[String] = {

    def dateCheck(rule: Rule): Boolean = rule match {
      case dateRule: DateRangeRule =>  {
        val diff = for (frmDt <- dateRule.fromDate; toDt <- dateRule.toDate) yield frmDt.isBefore(toDt) || frmDt.isEqual(toDt)

        diff match {
          case scala.util.Success(false) => false
          case scala.util.Success(true) => true
          case scala.util.Failure(_) => false
        }
      }
      case _ => true
    }

    val v = for {
      cd <- columnDefinitions
      rule <- cd.rules
      if (!dateCheck(rule))
    } yield s"""Column: ${cd.id}: Invalid ${rule.toError}: at line: ${rule.pos.line}, column: ${rule.pos.column}"""

    if (v.isEmpty) None else Some(v.mkString("\n"))
  }

  private def uniqueMultiValid(columnDefinitions: List[ColumnDefinition]): Option[String] = {
    def uniqueMultiCheck(rule: Rule): Option[List[String]] = rule match {
      case UniqueMultiRule(columns) =>
        val actualColumns:List[String] =  columnDefinitions.map( _.id)
        val invalidColumn = columns.filterNot( f => actualColumns.exists(_ == f))

        if ( invalidColumn.isEmpty) None else Some(invalidColumn)

      case _ => None
    }

    val v = for {
      cd <- columnDefinitions
      rule <- cd.rules
      invalidColumns = uniqueMultiCheck(rule)
      _ <- invalidColumns
    } yield s"""Column: ${cd.id}: Invalid cross reference ${invalidColumns.get.mkString("$", ", $", "")}: at line: ${rule.pos.line}, column: ${rule.pos.column}"""

    if (v.isEmpty) None else Some(v.mkString("\n"))
  }
}