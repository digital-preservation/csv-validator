/**
 * Copyright (c) 2013, The National Archives <digitalpreservation@nationalarchives.gov.uk>
 * http://www.nationalarchives.gov.uk
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package uk.gov.nationalarchives.csv.validator.schema

import scala.util.parsing.combinator._
import java.io.Reader
import scala.util.Try
import collection.immutable.TreeMap
import java.security.MessageDigest
import scalaz._
import Scalaz._
import uk.gov.nationalarchives.csv.validator.EOL
import scala.util.parsing.input.{OffsetPosition, Position}
import uk.gov.nationalarchives.csv.validator.{SchemaMessage, FailMessage}

trait SchemaParser extends RegexParsers {

  override protected val whiteSpace = """[ \t]*""".r

  //val white: Parser[String] = whiteSpace

  val eol = """\r?\n""".r

  val charPattern = """([^"\p{Cntrl}]|\\[\\'"bfnrt]|\\u[a-fA-F0-9]{4})"""

  //val stringRegex = """([^"\p{Cntrl}\\]|\\[\\'"bfnrt]|\\u[a-fA-F0-9]{4})*""".r
  val stringRegex = s"""$charPattern*""".r     //allow un-escaped '\'
  val charRegex = charPattern.r

  val pathSubstitutions: List[(String,String)]

  val enforceCaseSensitivePathChecks: Boolean

  def parseAndValidate(reader: Reader): ValidationNel[FailMessage, Schema] = {

    //TODO following function works around a deficiency in scala.util.parsing.combinator.Parsers{$Error, $Failure} that use hard-coded Unix EOL in Scala 2.10.0
    def formatNoSuccessMessageForPlatform(s: String) = {
      if(sys.props("os.name").toLowerCase.startsWith("win"))
        s.replaceAll("([^\\r]?)\\n", "$1\r\n")
      else
        s
    }

    parse(reader) match {
      case s @ Success(schema: Schema, next) => {
        val errors = validate(schema.globalDirectives, schema.columnDefinitions)
        if (errors.isEmpty) schema.successNel[FailMessage] else SchemaMessage(errors).failNel[Schema]
      }
      case n: NoSuccess => SchemaMessage(formatNoSuccessMessageForPlatform(n.toString)).failNel[Schema]
    }
  }

  def parse(reader: Reader) = parseAll(schema, reader)

  def schema = prolog ~ body ^^ {
    case version ~ globalDirectives ~ columnDefs =>
      Schema(globalDirectives, columnDefs)
  }

  def prolog = versionDecl ~ globalDirectives

  def versionDecl: Parser[String] = ("version" ~> Schema.version <~ eol).withFailureMessage(s"version ${Schema.version} missing or incorrect")

  def globalDirectives: Parser[List[GlobalDirective]] = rep(positioned(globalDirective <~ opt(eol)))

  def globalDirective = separatorDirective | quotedDirective | totalColumnsDirective | (noHeaderDirective | ignoreColumnNameCaseDirective)

  def separatorDirective: Parser[Separator] = "@separator" ~> (separatorTabExpr | separatorChar)

  def separatorTabExpr: Parser[Separator] = ("TAB" | """'\t'""") ^^^ Separator('\t')

  def separatorChar: Parser[Separator] =  characterLiteral ^^ { Separator(_) }

  def quotedDirective: Parser[Quoted] = "@quoted" ^^^ Quoted()

  def totalColumnsDirective: Parser[TotalColumns] = ("@totalColumns" ~> positiveNonZeroIntegerLiteral ^^ { TotalColumns(_) } ).withFailureMessage("@totalColumns invalid")

  def noHeaderDirective: Parser[NoHeader] = "@noHeader" ^^^ NoHeader()

  def ignoreColumnNameCaseDirective: Parser[IgnoreColumnNameCase] = "@ignoreColumnNameCase" ^^^ IgnoreColumnNameCase()

  def body = rep1(bodyPart) <~ rep(eol)

  def bodyPart = (rep(comment) ~> columnDefinitions) <~ rep(comment)

  def comment: Parser[Any] = singleLineComment | multiLineComment

  def singleLineComment: Parser[String] = """//[\S\t ]*(?:\r?\n)?""".r

  def multiLineComment: Parser[String] = """\/\*(?:[^*\r\n]+|(?:\r?\n))*\*\/(?:\r?\n)?""".r

  def columnDefinitions = (positioned(columnDefinition))
  //def columnDefinitions = columnDefinition

  def columnDefinition: Parser[ColumnDefinition] = (
    ((columnIdentifier | quotedColumnIdentifier) <~ ":") ~ columnRule <~ (endOfColumnDefinition | comment) ^^ {
      case id ~ (rules ~ columnDirectives) =>
        ColumnDefinition(id, rules, columnDirectives)
    }
  ).withFailureMessage("Invalid column definition")


  def columnIdentifier = (positiveNonZeroIntegerLiteral | ident).withFailureMessage("Column identifier invalid") ^^ {
    _.toString //TODO should be able to remove this `.toString` in favour of `id` field in ColumnDefinition being either NamedId <: String or OffsetId <: Integer, rather than just a String! see columnDefinition parser extractor above
  }

  def quotedColumnIdentifier = stringLiteral withFailureMessage("Quoted column identifier invalid")

  def columnRule = rep(columnValidationExpr) ~ columnDirectives

  //def columnRule = rep(columnValidationExpr)

  //TODO refactor into columnDirective and update EBNF
  def columnDirectives: Parser[List[ColumnDirective]] = rep(positioned(optionalDirective | ignoreCaseDirective | warningDirective))

  def optionalDirective = "@optional" ^^^ Optional()

  def ignoreCaseDirective = "@ignoreCase" ^^^ IgnoreCase()

  def warningDirective = "@warning" ^^^ Warning()

  //def columnValidationExpr = positioned(combinatorialAndNonConditionalRule | conditionalRule)
  //def combinatorialAndNonConditionalRule = (and | or | nonConditionalRule)
  //def nonConditionalRule = opt( columnRef <~ "/") ~ unaryRule ^^ { case explicitColumn ~ rule => rule.explicitColumn = explicitColumn; rule }

  def columnValidationExpr: Parser[Rule] = positioned(combinatorialExpr | nonCombinatorialExpr)
  //def columnValidationExpr: Parser[Rule] = combinatorialExpr | nonCombinatorialExpr //TODO reinstate positioned above

  def nonCombinatorialExpr = nonConditionalExpr | conditionalExpr

  //TODO combinatorialExpr causes a StackOverflowException

  def nonConditionalExpr: Parser[Rule] = singleExpr | externalSingleExpr | parenthesizedExpr

  def singleExpr: Parser[Rule] = opt(explicitContextExpr) ~
    (isExpr | notExpr | inExpr |
      startsWithExpr | endsWithExpr | regExpExpr |
      rangeExpr | lengthExpr |
      emptyExpr | notEmptyExpr | uniqueExpr |
      uriExpr |
      xsdDateTimeExpr | xsdDateExpr | xsdTimeExpr |
      ukDateExpr | partialUkDateExpr |
      uuid4Expr |
      positiveIntegerExpr) ^^ {
    case explicitContext ~ rule =>
      rule.explicitColumn = explicitContext
      rule
  }

  def explicitContextExpr = columnRef <~ "/"

  //def argProvider: Parser[ArgProvider] = columnRef ^^ { s => ColumnReference(s) } | '\"' ~> stringRegex <~ '\"' ^^ {s => Literal(Some(s)) }

  def columnRef: Parser[ColumnReference] = "$" ~> (columnIdentifier | quotedColumnIdentifier) ^^ { ColumnReference(_) }

  def isExpr: Parser[IsRule] = "is(" ~> stringProvider <~ ")" ^^ { IsRule }

  def notExpr: Parser[NotRule] = "not(" ~> stringProvider <~ ")" ^^ { NotRule }

  def inExpr: Parser[InRule] = "in(" ~> stringProvider <~ ")" ^^ { InRule }

  def startsWithExpr: Parser[StartsRule] = "starts(" ~> stringProvider <~ ")" ^^ { StartsRule }

  def endsWithExpr: Parser[EndsRule] = "ends(" ~> stringProvider <~ ")" ^^ { EndsRule }

  //TODO could improve error or regex?
  //TODO How to escape quotes inside regex?
  //def regExpExpr: Parser[RegExpRule] = "regex(" ~> stringLiteral <~ ")" ^^ { RegExpRule }
  //def regExpExpr: Parser[RegExpRule] = "regex" ~> """\(".+"\)""".r ^^ {
  def regExpExpr: Parser[RegExpRule] = "regex" ~> """([(]")(.*?)("[)])""".r ^^ {
    case s =>
      RegExpRule(s.dropRight(2).drop(2))
  } withFailureMessage("""regex not correctly delimited as ("your regex")""")

  //def regex = "regex" ~> regexParser ^^ { s => RegexRule(s.dropRight(2).drop(2)) }
  //val Regex = """([(]")(.*?)("[)])""".r
  //val regexParser: Parser[String] = Regex withFailureMessage("""regex not correctly delimited as ("your regex")""")

  def rangeExpr: Parser[RangeRule] = "range(" ~> numericLiteral ~ "," ~ numericLiteral <~ ")"  ^^ { case a ~ "," ~ b =>  RangeRule(a, b) }

  //TODO refactor
  def lengthExpr: Parser[LengthRule] = ("length(" ~> opt(positiveIntegerOrAny <~ ",") ~ positiveIntegerOrAny <~ ")") ^^ {
    case from ~ to => LengthRule(from, to)
  }

  def positiveIntegerOrAny = (positiveIntegerLiteral | wildcardLiteral) ^^ {
    _.toString //TODO should be able to remove this `.toString` in favour of `LengthRule` in lengthExpr being either AnyLength Object or BigInt, rather than just a String!
  }

  def emptyExpr = "empty" ^^^ EmptyRule()

  def notEmptyExpr = "notEmpty" ^^^ NotEmptyRule()

  //TODO rewrite uniqueExpr and uniqueMultiExpr into single expression, will need to refactor UniqueRule and UniqueMultiRule
  //def uniqueExpr: Parser[UniqueRule] = "unique" ^^^ UniqueRule()
  //def uniqueMultiExpr: Parser[UniqueMultiRule] = "unique(" ~> columnRef ~ rep(',' ~> columnRef) <~ ")" ^^ { s => UniqueMultiRule( s._1 :: s._2 ) }
  def uniqueExpr: Parser[Rule] = "unique" ~> opt("(" ~> columnRef ~ rep(',' ~> columnRef) <~ ")") ^^ {
    case None =>
      UniqueRule()
    case Some((columnRef1 ~ columnRefN)) =>
      UniqueMultiRule(columnRef1 :: columnRefN)
  }


  def uriExpr = "uri" ^^^ UriRule()

  //TODO adjust EBNF in spec so that xsdDateTime xsdDate and xsdTime literals do not need to be quoted etc. I see no point in forcing users to put them in quotes!

  //TODO below

  //<editor-fold desc="XsdDateTimeExpr">
  def xsdDateTimeExpr =  xDateTimeRange | xDateTime

  def xDateTime: Parser[XsdDateTimeRule] = "xDateTime" ^^^ XsdDateTimeRule()

  def xDateTimeRange: Parser[XsdDateTimeRangeRule] = "xDateTime(" ~> xDateTimeExpr ~ "," ~ xDateTimeExpr <~ ")" ^^  {
    case from ~ "," ~ to => XsdDateTimeRangeRule(from, to)
  }

  //TODO change to literal
  def xDateTimeExpr: Parser[String] = """[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}""".r
  //</editor-fold>

  //<editor-fold desc="XsdDateExpr">
  def xsdDateExpr = xDateRange | xDate

  def xDate: Parser[XsdDateRule] = "xDate" ^^^ XsdDateRule()

  def xDateRange: Parser[XsdDateRangeRule] = "xDate(" ~> xDateExpr ~ "," ~ xDateExpr <~ ")" ^^  {
    case from ~ "," ~ to => XsdDateRangeRule(from, to)
  }

  //TODO change to literal
  def xDateExpr: Parser[String] = "[0-9]{4}-[0-9]{2}-[0-9]{2}".r
  //</editor-fold>

  //<editor-fold desc="XsdTimeExpr">
  def xsdTimeExpr = xTimeRange | xTime

  def xTime: Parser[XsdTimeRule] = "xTime" ^^^ XsdTimeRule()

  def xTimeRange: Parser[XsdTimeRangeRule] = "xTime(" ~> xTimeExpr ~ "," ~ xTimeExpr <~ ")" ^^  {
    case from ~ "," ~ to => XsdTimeRangeRule(from, to)
  }

  //TODO change to literal
  val xTimeExpr: Parser[String] = "[0-9]{2}:[0-9]{2}:[0-9]{2}".r
  //</editor-fold>

  //<editor-fold desc="ukDateExpr">
  def ukDateExpr = ukDateRange | ukDate

  def ukDate: Parser[UkDateRule] = "ukDate" ^^^ UkDateRule()

  def ukDateRange: Parser[UkDateRangeRule] = "ukDate(" ~> ukDateStringLiteral ~ "," ~ ukDateStringLiteral <~ ")" ^^  {
    case from ~ "," ~ to => UkDateRangeRule(from, to)
  }

  //TODO change to literal
  val ukDateStringLiteral: Parser[String] = "[0-9]{1,2}/[0-9]{1,2}/[0-9]{4}".r
  //</editor-fold>

  //TODO missing DateExpr

  def partialUkDateExpr: Parser[PartUkDateRule] = "partUkDate" ^^^ PartUkDateRule()

  //TODO missing PartialDateExpr

  def uuid4Expr: Parser[Uuid4Rule] = "uuid4" ^^^ Uuid4Rule()

  def positiveIntegerExpr: Parser[PositiveIntegerRule] = "positiveInteger" ^^^ PositiveIntegerRule()

  def stringProvider: Parser[ArgProvider] = columnRef | (stringLiteral ^^ { s => Literal(Some(s)) } )

  def externalSingleExpr: Parser[Rule] = opt(explicitContextExpr) ~ (fileExistsExpr | checksumExpr | fileCountExpr) ^^ {
    case explicitContext ~ rule =>
      rule.explicitColumn = explicitContext
      rule
  }

  //TODO below **** MUCH TODO!

  //TODO update EBNF to use simpler lexical rules
  //<editor-fold desc="lexical">
  def wildcardLiteral = "*"

  def numericLiteral: Parser[BigDecimal] = number.r ^^ { BigDecimal(_) }
  val number = """[\-]?[0-9]+(?:\.[0-9]+)?"""

  def stringLiteral: Parser[String] = "\"" ~> stringPattern.r <~ "\""
  val stringPattern = """[^"]*"""

  def ident: Parser[String] = identPattern.r
  val identPattern = """[A-Za-z0-9\-_\.]+"""

  def positiveIntegerLiteral: Parser[BigInt] = positiveIntegerPattern.r ^^ { BigInt(_) }
  val positiveIntegerPattern = """[0-9]+"""

  def positiveNonZeroIntegerLiteral: Parser[BigInt] = positiveNonZeroIntegerPattern.r ^^ { BigInt(_) }
  val positiveNonZeroIntegerPattern = "[1-9][0-9]*"

  def characterLiteral: Parser[Char] =  "'" ~> nonBreakingChar <~ "'" ^^ { _.head }

  def nonBreakingChar = nonBreakingCharPattern.r
  val nonBreakingCharPattern = """[^\r\n\f]"""
  //</editor-fold>

  def combinatorialExpr = orExpr | andExpr

//  def unaryRule =
//    parenthesesRule | in | is | isNot | starts | ends |
//    empty | notEmpty |
//    uniqueMultiExpr | uniqueExpr |
//    regex | uuid4 | uri | xDateTimeRange | xDateTime | xDateRange | xDate | ukDateRange | ukDate | partUkDate | xTimeRange | xTime |
//    fileExists | checksum | fileCount |
//    positiveInteger | range | lengthExpr | failure("Invalid rule")

  def parenthesizedExpr: Parser[ParenthesesRule] = "(" ~> rep1(columnValidationExpr) <~ ")" ^^ { ParenthesesRule } | failure("unmatched paren")

  def conditionalExpr: Parser[Rule] = ifExpr
  
  //def orExpr: Parser[OrRule] = columnValidationExpr ~ "or" ~ columnValidationExpr  ^^ { case lhs ~ "or" ~ rhs => OrRule(lhs, rhs) }

  //def andExpr: Parser[AndRule] = columnValidationExpr ~ "and" ~ columnValidationExpr  ^^  { case lhs ~ "and" ~ rhs =>  AndRule(lhs, rhs) }

  def orExpr: Parser[OrRule] = nonCombinatorialExpr ~ "or" ~ columnValidationExpr  ^^ { case lhs ~ "or" ~ rhs => OrRule(lhs, rhs) }

  def andExpr: Parser[AndRule] = nonCombinatorialExpr ~ "and" ~ columnValidationExpr  ^^  { case lhs ~ "and" ~ rhs =>  AndRule(lhs, rhs) }



//  def ifExpr: Parser[IfRule] = (("if(" ~> white ~> combinatorialAndNonConditionalRule <~ white <~ "," <~ white) ~ (rep1(columnValidationExpr)) ~ opt((white ~> "," ~> white ~> rep1(columnValidationExpr))) <~ white <~ ")" ^^ {
//    case cond ~ bdy ~ optBdy => IfRule(cond, bdy, optBdy)
//  }) | failure("Invalid rule")

  //TODO update EBNF to match this
  def ifExpr: Parser[IfRule] = (("if(" ~> (combinatorialExpr | nonConditionalExpr) <~ ",") ~ rep1(columnValidationExpr) ~ opt("," ~> rep1(columnValidationExpr)) <~ ")" ^^ {
    case cond ~ bdy ~ optBdy => IfRule(cond, bdy, optBdy)
  }) | failure("Invalid rule")


//  def argProvider: Parser[ArgProvider] = columnRef ^^ { s => ColumnReference(s) } | '\"' ~> stringRegex <~ '\"' ^^ {s => Literal(Some(s)) }

  //def fileArgProvider: Parser[ArgProvider] = columnRef ^^ { s => ColumnReference(s) } | '\"' ~> rootFilePath <~ '\"' ^^ {s => Literal(Some(s)) }

  //TODO refactor into single expression
  def fileExistsExpr = ("fileExists(" ~> stringProvider <~ ")" ^^ { s => FileExistsRule(pathSubstitutions, enforceCaseSensitivePathChecks, s) }).withFailureMessage("fileExists rule has an invalid file path") |
    "fileExists" ^^^ { FileExistsRule( pathSubstitutions, enforceCaseSensitivePathChecks ) } | failure("Invalid fileExists rule")

  def rootFilePath: Parser[String] = """[\^&'@\{\}\[\]\,\$=!\-#\(\)%\.\+~_a-zA-Z0-9\s\\/:]+""".r    //Characters taken from http://support.microsoft.com/kb/177506 and added '/' and '\' and ':'

  def checksumExpr = "checksum(" ~> file ~ "," ~ algorithmExpr <~ ")" ^^ { case files ~ "," ~ algorithm => ChecksumRule(files._1.getOrElse(Literal(None)), files._2, algorithm, pathSubstitutions, enforceCaseSensitivePathChecks) }

  def fileCountExpr = "fileCount(" ~> file <~ ")" ^^ { case a  => FileCountRule(a._1.getOrElse(Literal(None)), a._2, pathSubstitutions) }

  def dateRange = "dateRange(\""

  def file = "file(" ~> opt(stringProvider <~ ",") ~ stringProvider <~ ")"

  def algorithmExpr: Parser[String] = "\"" ~> stringRegex <~ "\""  ^^ { a => a }

  private def endOfColumnDefinition: Parser[Any] = whiteSpace ~ (eol | endOfInput | failure("Invalid column definition"))

  private def endOfInput: Parser[Any] = new Parser[Any] {
    def apply(input: Input) = {
      if (input.atEnd) new Success("End of Input reached", input)
      else Failure("End of Input expected", input)
    }
  }

  private def validate(g: List[GlobalDirective], c: List[ColumnDefinition]): String = {
    globDirectivesValid(g) ::totalColumnsValid(g, c) :: columnDirectivesValid(c) :: duplicateColumnsValid(c) :: crossColumnsValid(c) :: checksumAlgorithmValid(c) ::
    rangeValid(c) :: lengthValid(c) :: regexValid(c) :: dateRangeValid(c) :: uniqueMultiValid(c) :: explicitColumnValid(c) :: Nil collect { case Some(s: String) => s } mkString(EOL)
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
    else Some(duplicates.map { case (id, cds) => s"""Column: $id has duplicates on lines """ + cds.map(cd => cd.pos.line).mkString(", ") }.mkString(EOL))
  }

  private def globDirectivesValid(directives: List[GlobalDirective]): Option[String] = {
    val duplicates = for ((name, lst) <- directives.groupBy(_.name) if (lst.length > 1)) yield {
      s"Global directive @$name is duplicated"
    }

    if (duplicates.isEmpty) None else Some(duplicates.mkString(EOL))
  }

  private def columnDirectivesValid(columnDefinitions: List[ColumnDefinition]): Option[String] = {
    val v = for {
      cd <- columnDefinitions
      if (cd.directives.distinct.length != cd.directives.length)
    } yield {
      s"${cd.id}: Duplicated column directives: " +
      cd.directives.groupBy(identity).filter { case (_, cds) => cds.size > 1}.map { case (cdId, _) => "@" + cdId + s" at line: ${cdId.pos.line}, column: ${cdId.pos.column}"}.mkString(", ")
    }

    if (v.isEmpty) None else Some(v.mkString(EOL))
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

    if (v.isEmpty) None else Some(v.mkString(EOL))
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

    if (v.isEmpty) None else Some(v.mkString(EOL))
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

    if (v.isEmpty) None else Some(v.mkString(EOL))
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
    else Some(errors.map { case (cd, rules) => s"Column: ${cd.id} has invalid ${crossReferenceErrors(rules)}" }.mkString(EOL))
  }

  private def regexValid(columnDefinitions: List[ColumnDefinition]): Option[String] = {
    def regexCheck(rule: Rule): Boolean = rule match {
      case RegExpRule(s) =>  Try(s.r).isFailure
      case _ => false
    }

    val result = for {
      cd <- columnDefinitions
      rule <- cd.rules
      if (regexCheck(rule))
    } yield s"""Column: ${cd.id}: Invalid ${rule.toError}: at line: ${rule.pos.line}, column: ${rule.pos.column}"""

    if (result.isEmpty) None else Some(result.mkString(EOL))
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

    val result = for {
      cd <- columnDefinitions
      rule <- cd.rules
      if (!dateCheck(rule))
    } yield s"""Column: ${cd.id}: Invalid ${rule.toError}: at line: ${rule.pos.line}, column: ${rule.pos.column}"""

    if (result.isEmpty) None else Some(result.mkString(EOL))
  }

  private def uniqueMultiValid(columnDefinitions: List[ColumnDefinition]): Option[String] = {
    def uniqueMultiCheck(rule: Rule): Option[List[String]] = rule match {
      case UniqueMultiRule(columns) =>
        val actualColumns: List[String] = columnDefinitions.map(_.id)
        val invalidColumns: List[ColumnReference] = columns.filterNot(f => actualColumns.exists(_ == f.value))

        if(invalidColumns.isEmpty)
          None
        else
          Some(invalidColumns.map(_.value))

      case _ =>
        None
    }

    val v = for {
      cd <- columnDefinitions
      rule <- cd.rules
      invalidColumns = uniqueMultiCheck(rule)
      _ <- invalidColumns
    } yield s"""Column: ${cd.id}: Invalid cross reference ${invalidColumns.get.mkString("$", ", $", "")}: at line: ${rule.pos.line}, column: ${rule.pos.column}"""

    if (v.isEmpty) None else Some(v.mkString(EOL))
  }

  private def explicitColumnValid(columnDefinitions: List[ColumnDefinition]): Option[String] = {

    def invalidColumnNames(rule:Rule) = explicitColumnCheck(rule) match {
      case Some(x) => x
      case None => List.empty[String]
    }

    def checkAlternativeOption(rules: Option[List[Rule]]): Option[List[String]] = rules match {
      case Some(rulesList) => Some( rulesList.foldLeft(List.empty[String]) {
        case (list, rule: Rule) =>  list ++ invalidColumnNames(rule)
      })

      case None => None
    }

    def explicitColumnCheck(rule: Rule): Option[List[String]] = rule match {
      case IfRule(c,t,f) =>
        val cond = explicitColumnCheck(c)
        val cons = t.foldLeft(Some(List.empty[String])) { case (l, r) => Some((l ++  explicitColumnCheck(r)).flatten.toList) }
        val alt = checkAlternativeOption(f)
        Some((cond ++ cons ++ alt).flatten.toList)

      case AndRule(lf,rt) =>
         val left = explicitColumnCheck(lf)
         val right = explicitColumnCheck(rt)
         Some((left ++ right ).flatten.toList)

      case OrRule(lf,rt) =>
         val left = explicitColumnCheck(lf)
         val right = explicitColumnCheck(rt)
         Some((left ++ right ).flatten.toList)

      case ParenthesesRule(l) =>
        l.foldLeft(Some(List.empty[String])) { case (l,r) => Some((l ++  explicitColumnCheck(r)).flatten.toList) }

      case _ =>
        rule.explicitColumn match {
          case Some(columnRef) if(!columnDefinitions.map(_.id).contains(columnRef.value)) =>
              Some(List(columnRef.value))
          case _ =>
            None
      }
    }

    val result = for {
      cd <- columnDefinitions
      rule <- cd.rules
      errorColumn = explicitColumnCheck(rule)
      if( errorColumn.isDefined && errorColumn.get.length > 0)
    } yield  s"""Column: ${cd.id}: Invalid explicit column ${errorColumn.get.mkString(", ")}: at line: ${rule.pos.line}, column: ${rule.pos.column}"""

    if (result.isEmpty) None else Some(result.mkString(EOL))
  }
}