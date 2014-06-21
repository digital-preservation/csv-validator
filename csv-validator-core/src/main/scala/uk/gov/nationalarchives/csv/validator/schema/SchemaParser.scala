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
import uk.gov.nationalarchives.csv.validator.{SchemaMessage, FailMessage}

/**
 * CSV Schema Parser
 *
 * Uses Scala Parser Combinators to parse the CSV Schema language defined in
 * the specification document
 * @see http://digital-preservation.github.io/csv-validator/csv-schema-1.0.html
 */
trait SchemaParser extends RegexParsers {

  /**
   * Any path substitutions needed when
   * resolving file paths
   */
  val pathSubstitutions: List[(String, String)]

  /**
   * Whether to enforce case sensitivity
   * in file path checks. Useful
   * when working on operating systems /
   * filesystems that ignore file path
   * case sensitivity, e.g. Windows and NTFS
   */
  val enforceCaseSensitivePathChecks: Boolean


  //<editor-fold desc="CSV Schema parser combinators">

  /**
   * [1] Schema ::= Prolog Body
   */
  def schema = prolog ~ body ^^ {
    case version ~ globalDirectives ~ columnDefs =>
      Schema(globalDirectives, columnDefs)
  }

  /**
   * [2] Prolog ::= VersionDecl GlobalDirectives
   */
  def prolog = versionDecl ~ globalDirectives

  /**
   * [3] VersionDecl ::= "version 1.0"
   */
  def versionDecl: Parser[String] = ("version" ~> Schema.version <~ eol).withFailureMessage(s"Schema version declaration 'version ${Schema.version}' missing or incorrect")

  /**
   * [4] GlobalDirectives	::=	SeparatorDirective? QuotedDirective? TotalColumnsDirective? (NoHeaderDirective | IgnoreColumnNameCaseDirective)?    /* expr: unordered */
   */
  def globalDirectives: Parser[List[GlobalDirective]] = opt(mingle(List(separatorDirective, quotedDirective, totalColumnsDirective, noHeaderDirective | ignoreColumnNameCaseDirective).map(positioned(_) <~ opt(eol)))
    .withFailureMessage("Invalid global directive")) ^^ { _.getOrElse(List.empty) }

  /**
   * [5] DirectivePrefix ::= "@"
   */
  def directivePrefix = "@"

  /**
   * [6] SeparatorDirective	::=	DirectivePrefix "separator" (SeparatorTabExpr | SeparatorChar)
   */
  def separatorDirective: Parser[Separator] = directivePrefix ~> "separator" ~> (separatorTabExpr | separatorChar)

  /**
   * [7] SeparatorTabExpr	::=	"TAB" | '\t'
   */
  def separatorTabExpr: Parser[Separator] = ("TAB" | """'\t'""") ^^^ Separator('\t')

  /**
   * [8] SeparatorChar ::= CharacterLiteral
   */
  def separatorChar: Parser[Separator] =  characterLiteral ^^ { Separator(_) }

  /**
   * [9] QuotedDirective ::=	DirectivePrefix "quoted"
   */
  def quotedDirective: Parser[Quoted] = directivePrefix ~> "quoted" ^^^ Quoted()

  /**
   * [10]	TotalColumnsDirective	::=	DirectivePrefix "totalColumns" PositiveNonZeroIntegerLiteral
   */
  def totalColumnsDirective: Parser[TotalColumns] = (directivePrefix ~> "totalColumns" ~> positiveNonZeroIntegerLiteral ^^ { TotalColumns(_) }).withFailureMessage("@totalColumns invalid")

  /**
   * [11]	NoHeaderDirective	::=	DirectivePrefix "noHeader"
   */
  def noHeaderDirective: Parser[NoHeader] = directivePrefix ~> "noHeader" ^^^ NoHeader()

  /**
   * [12]	IgnoreColumnNameCaseDirective	::=	DirectivePrefix "ignoreColumnNameCase"
   */
  def ignoreColumnNameCaseDirective: Parser[IgnoreColumnNameCase] = directivePrefix ~> "ignoreColumnNameCase" ^^^ IgnoreColumnNameCase()

  /**
   * [13]	Body ::= BodyPart+
   */
  def body = rep1(bodyPart) <~ rep(eol)

  /**
   * [14]	BodyPart ::= Comment* ColumnDefinition Comment*
   */
  def bodyPart = (rep(comment) ~> columnDefinition) <~ rep(comment)

  /**
   * [15]	Comment	::=	SingleLineComment | MultiLineComment
   */
  def comment: Parser[Any] = singleLineComment | multiLineComment

  /**
   * [16]	SingleLineComment	::=	"//" NonBreakingChar*
   */
  def singleLineComment: Parser[String] = """//[\S\t ]*(?:\r?\n)?""".r

  /**
   * [17]	MultiLineComment ::= "/*" Char* "*/"
   */
  def multiLineComment: Parser[String] = """\/\*(?:[^*\r\n]+|(?:\r?\n))*\*\/(?:\r?\n)?""".r

  /**
   * [18]	ColumnDefinition ::=	(ColumnIdentifier | QuotedColumnIdentifier) ":" ColumnRule
   */
  def columnDefinition: Parser[ColumnDefinition] = positioned((
    ((columnIdentifier | quotedColumnIdentifier) <~ ":") ~ columnRule <~ (endOfColumnDefinition | comment) ^^ {
      case id ~ (rules ~ columnDirectives) =>
        ColumnDefinition(id, rules, columnDirectives)
    }
  ).withFailureMessage("Invalid column definition"))

  /**
   * [19]	ColumnIdentifier ::= PositiveNonZeroIntegerLiteral | Ident
   */
  def columnIdentifier = (positiveNonZeroIntegerLiteral | ident).withFailureMessage("Column identifier invalid") ^^ {
    _.toString //TODO should be able to remove this `.toString` in favour of `id` field in ColumnDefinition being either NamedId <: String or OffsetId <: Integer, rather than just a String! see columnDefinition parser extractor above
  }

  /**
   * [20]	QuotedColumnIdentifier ::= StringLiteral
   */
  def quotedColumnIdentifier = stringLiteral withFailureMessage("Quoted column identifier invalid")

  /**
   * [21]	ColumnRule ::= ColumnValidationExpr* ColumnDirectives
   */
  def columnRule = rep(columnValidationExpr) ~ columnDirectives

  /**
   * [22]	ColumnDirectives ::= OptionalDirective? MatchIsFalseDirective? IgnoreCaseDirective? WarningDirective?    /* expr: unordered */
   */
  def columnDirectives: Parser[List[ColumnDirective]] = opt(mingle(List(optionalDirective, matchIsFalseDirective, ignoreCaseDirective, warningDirective).map(positioned(_)))
    .withFailureMessage("Invalid column directive")) ^^ { _.getOrElse(List.empty) }

  /**
   * [23]	OptionalDirective ::= DirectivePrefix "optional"
   */
  def optionalDirective = directivePrefix ~> "optional" ^^^ Optional()

  /**
   * [24] MatchIsFalseDirective	::=	DirectivePrefix "matchIsFalse"
   */
  //TODO implement workings of matchIsFalseDirective at present it does nothing!
  def matchIsFalseDirective = directivePrefix ~> "matchIsFalse" ^^^ MatchIsFalse()

  /**
   * [25] IgnoreCaseDirective	::=	DirectivePrefix "ignoreCase"
   */
  def ignoreCaseDirective = directivePrefix ~> "ignoreCase" ^^^ IgnoreCase()

  /**
   * [26]	WarningDirective ::=	DirectivePrefix "warningDirective"
   */
  def warningDirective = directivePrefix ~> "warning" ^^^ Warning()

  /**
   * [27]	ColumnValidationExpr ::= CombinatorialExpr | NonCombinatorialExpr
   */
  def columnValidationExpr: Parser[Rule] = positioned(combinatorialExpr | nonCombinatorialExpr)

  /**
   * [28]	CombinatorialExpr ::= OrExpr | AndExpr
   */
  def combinatorialExpr = orExpr | andExpr

  /**
   * [29]	OrExpr	::=	nonCombinatorialExpr "or" columnValidationExpr
   *
   * Uses nonCombinatorialExpr on the left-hand-side
   * to avoid left recursive rule
   */
  def orExpr: Parser[OrRule] = nonCombinatorialExpr ~ "or" ~ columnValidationExpr  ^^ { case lhs ~ "or" ~ rhs => OrRule(lhs, rhs) }

  /**
   * [30]	AndExpr	::=	nonCombinatorialExpr "and" ColumnValidationExpr
   *
   * Uses nonCombinatorialExpr on the left-hand-side
   * to avoid left recursive rule
   */
  def andExpr: Parser[AndRule] = nonCombinatorialExpr ~ "and" ~ columnValidationExpr  ^^  { case lhs ~ "and" ~ rhs =>  AndRule(lhs, rhs) }

  /**
   * [31]	NonCombinatorialExpr ::= NonConditionalExpr | ConditionalExpr
   */
  def nonCombinatorialExpr = nonConditionalExpr | conditionalExpr

  /**
   * [32] NonConditionalExpr ::=	SingleExpr | ExternalSingleExpr | ParenthesizedExpr
   */
  def nonConditionalExpr: Parser[Rule] = singleExpr | externalSingleExpr | parenthesizedExpr

  /**
   * [33] SingleExpr ::=	ExplicitContextExpr? (IsExpr | NotExpr | InExpr |
   *                        StartsWithExpr | EndsWithExpr | RegExpExpr |
   *                        RangeExpr | LengthExpr |
   *                        EmptyExpr | NotEmptyExpr | UniqueExpr |
   *                        UriExpr |
   *                        XsdDateTimeExpr | XsdDateExpr | XsdTimeExpr |
   *                        UkDateExpr | DateExpr | PartialUkDateExpr | PartialDateExpr |
   *                        uuid4Expr |
   *                        PositiveIntegerExpr)
   */
  //TODO need to implement and add DateExpr, PartialDateExpr
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

  /**
   * [34]	ExplicitContextExpr	::=	ColumnRef "/"
   */
  def explicitContextExpr = columnRef <~ "/"

  /**
   * [35]	ColumnRef	::=	"$" (ColumnIdentifier | QuotedColumnIdentifier)
   */
  def columnRef: Parser[ColumnReference] = "$" ~> (columnIdentifier | quotedColumnIdentifier) ^^ { ColumnReference(_) }

  /**
   * [36]	IsExpr ::= "is(" StringProvider ")"
   */
  def isExpr: Parser[IsRule] = "is(" ~> stringProvider <~ ")" ^^ { IsRule }

  /**
   * [37]	NotExpr	::=	"not(" StringProvider ")"
   */
  def notExpr: Parser[NotRule] = "not(" ~> stringProvider <~ ")" ^^ { NotRule }

  /**
   * [38]	InExpr ::=	"in(" StringProvider ")"
   */
  def inExpr: Parser[InRule] = "in(" ~> stringProvider <~ ")" ^^ { InRule }

  /**
   * [39]	StartsWithExpr ::= "starts(" StringProvider ")"
   */
  def startsWithExpr: Parser[StartsRule] = "starts(" ~> stringProvider <~ ")" ^^ { StartsRule }

  /**
   * [40]	EndsWithExpr ::=	"ends(" StringProvider ")"
   */
  def endsWithExpr: Parser[EndsRule] = "ends(" ~> stringProvider <~ ")" ^^ { EndsRule }

  /**
   * [41]	RegExpExpr	::=	"regex(" StringLiteral ")"
   */
  //TODO could improve error or regex?
  //TODO How to escape quotes inside regex?
  def regExpExpr: Parser[RegExpRule] = "regex" ~> """([(]")(.*?)("[)])""".r ^^ {
    case s =>
      RegExpRule(s.dropRight(2).drop(2))
  } withFailureMessage("""regex not correctly delimited as ("your regex")""")

  /**
   * [42]	RangeExpr	::=	"range(" NumericLiteral "," NumericLiteral ")" /* range is inclusive */
   */
  def rangeExpr: Parser[RangeRule] = "range(" ~> numericLiteral ~ "," ~ numericLiteral <~ ")"  ^^ {
    case a ~ "," ~ b =>
      RangeRule(a, b)
  }

  /**
   * [43]	LengthExpr	::=	"length(" (PositiveIntegerOrAny ",")? PositiveIntegerOrAny ")"
   *
   * /*
   * length has 4 forms.
   * 1) length(n) ensures the value is: the exact length n (absolute length)
   * 2) length(n, *) ensures the value is: longer than or equal to n (minimum length)
   * 3) length(*, n) ensures the value is: shorter than or equal to n (maximum length)
   * 4) length(n1, n2) ensures the value is: longer than or equal to n1 AND shorter than or equal to n2 (minumum and maximum lengths)
   * */
   */
  def lengthExpr: Parser[LengthRule] = ("length(" ~> opt(positiveIntegerOrAny <~ ",") ~ positiveIntegerOrAny <~ ")") ^^ {
    case from ~ to =>
      LengthRule(from, to)
  }

  /**
   * [44]	PositiveIntegerOrAny ::=	PositiveIntegerLiteral | WildcardLiteral
   */
  def positiveIntegerOrAny: Parser[String] = (positiveIntegerLiteral | wildcardLiteral) ^^ {
    _.toString //TODO should be able to remove this `.toString` in favour of positiveIntegerOrAny returning some sort of number or any object, `LengthRule` in lengthExpr being either AnyLength object or BigInt, rather than just a String!
  }

  /**
   * [45]	EmptyExpr	::=	"empty"
   */
  def emptyExpr = "empty" ^^^ EmptyRule()

  /**
   * [46]	NotEmptyExpr ::=	"notEmpty"
   */
  def notEmptyExpr = "notEmpty" ^^^ NotEmptyRule()

  /**
   * [47]	UniqueExpr ::=	"unique" ("(" ColumnRef ("," ColumnRef)* ")")?
   */
  def uniqueExpr: Parser[Rule] = "unique" ~> opt("(" ~> columnRef ~ rep("," ~> columnRef) <~ ")") ^^ {
    case None =>
      UniqueRule()
    case Some((columnRef1 ~ columnRefN)) =>
      UniqueMultiRule(columnRef1 :: columnRefN)
  }

  /**
   * [48]	UriExpr	::=	"uri"
   */
  def uriExpr = "uri" ^^^ UriRule()

  /**
   * [49] XsdDateTimeExpr	::=	"xDateTime" ("(" XsdDateTimeLiteral "," XsdDateTimeLiteral ")")?
   */
  def xsdDateTimeExpr = "xDateTime" ~> opt((("(" ~> xsdDateTimeLiteral) <~ ",") ~ (xsdDateTimeLiteral <~ ")")) ^^ {
    case None =>
      XsdDateTimeRule()
    case Some((from ~ to)) =>
      XsdDateTimeRangeRule(from, to)
  }

  /**
   * [50] XsdDateExpr ::=	"xDate" ("(" XsdDateLiteral "," XsdDateLiteral ")")?
   */
  def xsdDateExpr = "xDate" ~> opt((("(" ~> xsdDateLiteral) <~ ",") ~ (xsdDateLiteral <~ ")")) ^^ {
    case None =>
      XsdDateRule()
    case Some((from ~ to)) =>
      XsdDateRangeRule(from, to)
  }

  /**
   * [51] XsdTimeExpr ::=	"xTime" ("(" XsdTimeLiteral "," XsdTimeLiteral ")")?
   */
  def xsdTimeExpr = "xTime" ~> opt((("(" ~> xsdTimeLiteral) <~ ",") ~ (xsdTimeLiteral <~ ")")) ^^ {
    case None =>
      XsdTimeRule()
    case Some((from ~ to)) =>
      XsdTimeRangeRule(from, to)
  }

  /**
   * [52] UkDateExpr ::=	"ukDate" ("(" UkDateLiteral "," UkDateLiteral ")")?
   */
  def ukDateExpr = "ukDate" ~> opt((("(" ~> ukDateLiteral) <~ ",") ~ (ukDateLiteral <~ ")")) ^^ {
    case None =>
      UkDateRule()
    case Some((from ~ to)) =>
      UkDateRangeRule(from, to)
  }

  /**
   * [53]	DateExpr ::=	"date(" StringProvider "," StringProvider "," StringProvider ("," XsdDateLiteral "," XsdDateLiteral)? ")"
   */
  //TODO implement DateExpr

  /**
   * [54]	PartialUkDateExpr	::=	"partUkDate"
   */
  def partialUkDateExpr: Parser[PartUkDateRule] = "partUkDate" ^^^ PartUkDateRule()

  /**
   * [55]	PartialDateExpr	::=	"partDate(" StringProvider "," StringProvider "," StringProvider ")"
   */
  //TODO implement PartialDateExpr

  /**
   * [56]	Uuid4Expr	::=	"uuid4"
   */
  def uuid4Expr: Parser[Uuid4Rule] = "uuid4" ^^^ Uuid4Rule()

  /**
   * [57]	PositiveIntegerExpr	::=	"positiveInteger"
   */
  def positiveIntegerExpr: Parser[PositiveIntegerRule] = "positiveInteger" ^^^ PositiveIntegerRule()

  /**
   * [58] StringProvider ::= ColumnRef | StringLiteral
   */
  def stringProvider: Parser[ArgProvider] = columnRef | (stringLiteral ^^ { s => Literal(Some(s)) } )

  /**
   * [59] ExternalSingleExpr ::= ExplicitContextExpr? (FileExistsExpr | ChecksumExpr | FileCountExpr)
   */
  def externalSingleExpr: Parser[Rule] = opt(explicitContextExpr) ~ (fileExistsExpr | checksumExpr | fileCountExpr) ^^ {
    case explicitContext ~ rule =>
      rule.explicitColumn = explicitContext
      rule
  }

  /**
   * [60] FileExistsExpr ::=	"fileExists" ("(" StringProvider ")")? /* optional path to prepend to this cell with filename in */
   */
  def fileExistsExpr = ("fileExists" ~> opt("(" ~> stringProvider <~ ")")).withFailureMessage("Invalid fileExists rule") ^^ {
    case None =>
      FileExistsRule(pathSubstitutions, enforceCaseSensitivePathChecks)
    case Some(s) =>
      FileExistsRule(pathSubstitutions, enforceCaseSensitivePathChecks, s)
  }

  /**
   * [61]	ChecksumExpr ::= "checksum(" FileExpr "," StringLiteral ")" /* first arg is file expr, second arg is algorithm to use for checksum */
   */
  def checksumExpr = ("checksum(" ~> fileExpr <~ ",") ~ stringLiteral <~ ")" ^^ {
    case files ~ algorithm =>
      ChecksumRule(files._1.getOrElse(Literal(None)), files._2, algorithm, pathSubstitutions, enforceCaseSensitivePathChecks)
  }

  /**
   * [62]	FileExpr ::= "file(" (StringProvider ",")? StringProvider ")" /* first (optional) arg is path (or ColumnRef of path) to prepend to second arg, second arg is filename (or ColumnRef of filename) */
   */
  def fileExpr = "file(" ~> opt(stringProvider <~ ",") ~ stringProvider <~ ")"

  /**
   * [63]	FileCountExpr	::=	"fileCount(" FileExpr ")"
   */
  def fileCountExpr = "fileCount(" ~> fileExpr <~ ")" ^^ {
    case a  =>
      FileCountRule(a._1.getOrElse(Literal(None)), a._2, pathSubstitutions)
  }

  /**
   * [64] ParenthesizedExpr ::=	"(" ColumnValidationExpr+ ")"
   */
  def parenthesizedExpr: Parser[ParenthesesRule] = "(" ~> rep1(columnValidationExpr) <~ ")" ^^ { ParenthesesRule } | failure("unmatched paren")

  /**
   * [65]	ConditionalExpr ::= IfExpr
   */
  def conditionalExpr: Parser[Rule] = ifExpr

  /**
   * [66] IfExpr ::= "if(" (CombinatorialExpr | NonConditionalExpr) "," ColumnValidationExpr+ ("," ColumnValidationExpr+)? ")" /* if with optional else */
   */
  def ifExpr: Parser[IfRule] = (("if(" ~> (combinatorialExpr | nonConditionalExpr) <~ ",") ~ rep1(columnValidationExpr) ~ opt("," ~> rep1(columnValidationExpr)) <~ ")" ^^ {
    case condition ~ thenExpr ~ elseExpr =>
      IfRule(condition, thenExpr, elseExpr)
  }) | failure("Invalid rule")

  private def endOfColumnDefinition: Parser[Any] = whiteSpace ~ (eol | endOfInput | failure("Invalid column definition"))

  private def endOfInput: Parser[Any] = new Parser[Any] {
    def apply(input: Input) = {
      if (input.atEnd) new Success("End of Input reached", input)
      else Failure("End of Input expected", input)
    }
  }
  //</editor-fold>

  //<editor-fold desc="CSV Schema lexical parser combinators">
  /**
   * [67] XsdDateTimeLiteral ::= XsdDateWithoutTimezoneComponent "T" XsdTimeLiteral
   */
  def xsdDateTimeLiteral: Parser[String] = (xsdDateWithoutTimezoneComponent + "T" + xsdTimeWithoutTimezoneComponent + xsdTimezoneComponent).r

  /**
   * [68]	XsdDateLiteral ::=	XsdDateWithoutTimezoneComponent XsdTimezoneComponent
   */
  def xsdDateLiteral: Parser[String] = (xsdDateWithoutTimezoneComponent + xsdTimezoneComponent).r

  /**
   * [69]	XsdTimeLiteral ::=	XsdTimeWithoutTimezoneComponent XsdTimezoneComponent
   */
  def xsdTimeLiteral: Parser[String] = (xsdTimeWithoutTimezoneComponent + xsdTimezoneComponent).r

  /**
   * [70]	XsdDateWithoutTimezoneComponent	::=	-?[0-9]{4}-(((0(1|3|5|7|8)|1(0|2))-(0[1-9]|(1|2)[0-9]|3[0-1]))|((0(4|6|9)|11)-(0[1-9]|(1|2)[0-9]|30))|(02-(0[1-9]|(1|2)[0-9])))    /* xgc:regular-expression */
   */
  //NOTE - we use a more relaxed regexp here than the spec, as another validation parse is done by the relevant rule class (e.g. schema.XsdTimeRegex, schema.XsdDateRegex or schema.XsdDateTimeRegex)
  lazy val xsdDateWithoutTimezoneComponent = "[0-9]{4}-[0-9]{2}-[0-9]{2}"

  /**
   * [71]	XsdTimeWithoutTimezoneComponent	::=	([0-1][0-9]|2[0-4]):(0[0-9]|[1-5][0-9]):(0[0-9]|[1-5][0-9])(\.[0-9]{3})?    /* xgc:regular-expression */
   */
  //NOTE - we use a more relaxed regexp here than the spec, as another validation parse is done by the relevant rule class (e.g. schema.XsdTimeRegex, schema.XsdDateRegex or schema.XsdDateTimeRegex)
  lazy val xsdTimeWithoutTimezoneComponent = """[0-9]{2}:[0-9]{2}:[0-9]{2}(\.[0-9]{3})?"""

  /**
   * [72]	XsdTimezoneComponent ::=	((\+|-)(0[1-9]|1[0-9]|2[0-4]):(0[0-9]|[1-5][0-9])|Z)?    /* xgc:regular-expression */
   */
  //NOTE - we use a more relaxed regexp here than the spec, as another validation parse is done by the relevant rule class (e.g. schema.XsdTimeRegex, schema.XsdDateRegex or schema.XsdDateTimeRegex)
  lazy val xsdTimezoneComponent = "(([+-][0-9]{2}:[0-9]{2})|Z)?"

  /**
   * [73]	UkDateLiteral	::=	(((0[1-9]|(1|2)[0-9]|3[0-1])\/(0(1|3|5|7|8)|1(0|2)))|((0[1-9]|(1|2)[0-9]|30)\/(0(4|6|9)|11))|((0[1-9]|(1|2)[0-9])\/02))\/[0-9]{4}
   */
  //NOTE - we use a more relaxed parser here than the spec, as another validation parse is done by schema.UkDateRegex in the appropriate rule
  def ukDateLiteral: Parser[String] = "[0-9]{1,2}/[0-9]{1,2}/[0-9]{4}".r

  /**
   * [74]	PositiveNonZeroIntegerLiteral	::=	[1-9][0-9]*     /* xgc:regular-expression */    /* A Natural Number, positive integer */
   */
  def positiveNonZeroIntegerLiteral: Parser[BigInt] = "[1-9][0-9]*".r ^^ { BigInt(_) }

  /**
   * [75]	PositiveIntegerLiteral ::= [0-9]+     /* xgc:regular-expression */    /* A Natural Number, non-negative integer */
   */
  def positiveIntegerLiteral: Parser[BigInt] = "[0-9]+".r ^^ { BigInt(_) }

  /**
   * [76] NumericLiteral ::= -?[0-9]+(\.[0-9]+)?         /* xgc:regular-expression */    /* A Real Number, expressed as an integer or decimal */
   */
  def numericLiteral: Parser[BigDecimal] = """-?[0-9]+(\.[0-9]+)?""".r ^^ { BigDecimal(_) }

  /**
   * [77] StringLiteral ::= "\"" [^"]* "\""    /* xgc:regular-expression */    /* Any characters except: quotation mark */
   */
  def stringLiteral: Parser[String] = "\"" ~> """[^"]*""".r <~ "\""

  /**
   * [78] CharacterLiteral ::= "'" [^\r\n\f'] "'"     /* xgc:regular-expression */    /* Any characters except: carriage-return, line-break, form-feed and apostrophe */
   */
  def characterLiteral: Parser[Char] =  "'" ~> """[^\r\n\f']""".r <~ "'" ^^ { _.head }

  /**
   * [79] WildcardLiteral ::= "*"
   */
  def wildcardLiteral = "*"

  /**
   * [80] Ident ::= [A-Za-z0-9\-_\.]+   /* xgc:regular-expression */
   */
  def ident: Parser[String] = """[A-Za-z0-9\-_\.]+""".r



  override protected val whiteSpace = """[ \t]*""".r

  private val eol = """\r?\n""".r

  /*
  private val charPattern = """([^"\p{Cntrl}]|\\[\\'"bfnrt]|\\u[a-fA-F0-9]{4})"""
  private val stringRegex = """([^"\p{Cntrl}\\]|\\[\\'"bfnrt]|\\u[a-fA-F0-9]{4})*""".r
  private val stringRegex = s"""$charPattern*""".r     //allow un-escaped '\'
  private val charRegex = charPattern.r
  */
  //</editor-fold>


  /**
   * Given 1 or more Parsers
   * this function produces
   * all permutations of
   * all combinations.
   *
   * Put more simply if you have a List
   * of Parsers, we create a Parser
   * that matches n of those parsers
   * in any order
   *
   * @param parsers A list of parsers to mingle
   * @return A parser that represents all permutations of
   *         all combinations of the parsers
   */
  private def mingle[T, U](parsers : List[Parser[T]]): Parser[List[T]] = {

    /**
     * All permutations of all combinations
     * of a List
     */
    def mingle[T](data: List[T]): List[List[T]] = {
      (for(i <- 1 to data.length) yield
        data.combinations(i).flatMap(_.permutations)
      ).toList.flatten
    }

    /**
     * Combines n parsers together
     * in the same manner as p1 ~ p2 ~ ... pN
     */
    def combine[T](parsers: List[Parser[T]]): Parser[List[T]] = {
      parsers.foldRight(success(List.empty[T])) {
        case (p, acc) => for {
          pRes <- p
          accRes <- acc
        } yield pRes :: accRes
      }
    }

    def longestFirst(l1: List[_], l2: List[_]) = l1.length > l2.length

    val mingled = mingle[Parser[T]](parsers)
      .sortWith(longestFirst)
    //we sort longest first here,
    //to make sure the parser that matches
    //the most input will always be put first

    val alternates = mingled.map(combine(_))
    alternates.reduceLeft(_ | _)
  }


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

    def invalidColumnNames(rule: Rule) = explicitColumnCheck(rule) match {
      case Some(x) => x
      case None => List.empty[String]
    }

    def checkAlternativeOption(rules: Option[List[Rule]]): Option[List[String]] = rules match {
      case Some(rulesList) => Some(rulesList.foldLeft(List.empty[String]) {
        case (list, rule: Rule) => list ++ invalidColumnNames(rule)
      })

      case None => None
    }

    def explicitColumnCheck(rule: Rule): Option[List[String]] = rule match {
      case IfRule(c, t, f) =>
        val cond = explicitColumnCheck(c)
        val cons = t.foldLeft(Some(List.empty[String])) { case (l, r) => Some((l ++ explicitColumnCheck(r)).flatten.toList)}
        val alt = checkAlternativeOption(f)
        Some((cond ++ cons ++ alt).flatten.toList)

      case AndRule(lf, rt) =>
        val left = explicitColumnCheck(lf)
        val right = explicitColumnCheck(rt)
        Some((left ++ right).flatten.toList)

      case OrRule(lf, rt) =>
        val left = explicitColumnCheck(lf)
        val right = explicitColumnCheck(rt)
        Some((left ++ right).flatten.toList)

      case ParenthesesRule(l) =>
        l.foldLeft(Some(List.empty[String])) { case (l, r) => Some((l ++ explicitColumnCheck(r)).flatten.toList)}

      case _ =>
        rule.explicitColumn match {
          case Some(columnRef) if (!columnDefinitions.map(_.id).contains(columnRef.value)) =>
            Some(List(columnRef.value))
          case _ =>
            None
        }
    }

    val result = for {
      cd <- columnDefinitions
      rule <- cd.rules
      errorColumn = explicitColumnCheck(rule)
      if (errorColumn.isDefined && errorColumn.get.length > 0)
    } yield s"""Column: ${cd.id}: Invalid explicit column ${errorColumn.get.mkString(", ")}: at line: ${rule.pos.line}, column: ${rule.pos.column}"""

    if (result.isEmpty) None else Some(result.mkString(EOL))
  }
}