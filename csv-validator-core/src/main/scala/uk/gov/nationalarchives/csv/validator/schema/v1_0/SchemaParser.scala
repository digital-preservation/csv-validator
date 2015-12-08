/**
 * Copyright (c) 2013, The National Archives <digitalpreservation@nationalarchives.gov.uk>
 * http://www.nationalarchives.gov.uk
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package uk.gov.nationalarchives.csv.validator.schema.v1_0

import java.io.Reader

import uk.gov.nationalarchives.csv.validator.schema.{SchemaParser => BaseSchemaParser, _}

/**
  * CSV Schema Parser
  *
  * Uses Scala Parser Combinators to parse the CSV Schema language defined in
  * the specification document
  * @see http://digital-preservation.github.io/csv-validator/csv-schema-1.0.html
  */
trait SchemaParser extends BaseSchemaParser {

  def parseVersionAware(reader: Reader): ParseResult[Schema] =
    parseAll(schema, reader)


  /**
    * [1] Schema ::= Prolog Body
    */
  lazy val schema: PackratParser[Schema] = "Schema" ::= prolog ~ body ^^ {
    case version ~ globalDirectives ~ columnDefs =>
      Schema(globalDirectives, columnDefs, version)
  }


  /**
    * [2] Prolog ::= VersionDecl GlobalDirectives
    */
  lazy val prolog = "Prolog" ::= versionDecl ~ globalDirectives

  /**
    * [3] VersionDecl ::= "version 1.0"
    */
  lazy val versionDecl: PackratParser[String] = "VersionDecl" ::= ("version" ~> "1.0" <~ eol).withFailureMessage(s"Schema version declaration 'version 1.0' missing or incorrect")



  /**
    * [4] GlobalDirectives	::=	SeparatorDirective? QuotedDirective? TotalColumnsDirective? PermitEmptyDirective? (NoHeaderDirective | IgnoreColumnNameCaseDirective)?   /* expr: unordered */
    */
  lazy val globalDirectives: PackratParser[List[GlobalDirective]] = "GlobalDirectives" ::= opt(mingle(List(separatorDirective, quotedDirective, totalColumnsDirective, permitEmptyDirective, noHeaderDirective | ignoreColumnNameCaseDirective).map(positioned(_) <~ opt(eol))).withFailureMessage("Invalid global directive")) ^^ {
    _.getOrElse(List.empty)
  }

  /**
    * [5] DirectivePrefix ::= "@"
    */
  lazy val directivePrefix = "DirectivePrefix" ::= "@"

  /**
    * [6] SeparatorDirective ::= DirectivePrefix "separator" (SeparatorTabExpr | SeparatorChar)
    */
  lazy val separatorDirective: PackratParser[Separator] = "SeparatorDirective" ::= directivePrefix ~> "separator" ~> (separatorTabExpr | separatorChar)

  /**
    * [7] SeparatorTabExpr ::= "TAB" | '\t'
    */
  lazy val separatorTabExpr: PackratParser[Separator] = "SeparatorTabExpr" ::= ("TAB" | """'\t'""") ^^^ Separator('\t')

  /**
    * [8] SeparatorChar ::= CharacterLiteral
    */
  lazy val separatorChar: PackratParser[Separator] = "SeparatorChar" ::= characterLiteral ^^ {
    Separator(_)
  }

  /**
    * [9] QuotedDirective ::=  DirectivePrefix "quoted"
    */
  lazy val quotedDirective: PackratParser[Quoted] = "QuotedDirective" ::= directivePrefix ~> "quoted" ^^^ Quoted()

  /**
    * [10] TotalColumnsDirective ::= DirectivePrefix "totalColumns" PositiveNonZeroIntegerLiteral
    */
  lazy val totalColumnsDirective: PackratParser[TotalColumns] = "TotalColumnsDirective" ::= (directivePrefix ~> "totalColumns" ~> positiveNonZeroIntegerLiteral ^^ {
    TotalColumns(_)
  }).withFailureMessage("@totalColumns invalid")

  /**
    * [11] NoHeaderDirective ::= DirectivePrefix "noHeader"
    */
  lazy val noHeaderDirective: PackratParser[NoHeader] = "NoHeaderDirective" ::= directivePrefix ~> "noHeader" ^^^ NoHeader()

  /**
    * [12] PermitEmptyDirective ::=  DirectivePrefix "permitEmpty"
    */
  lazy val permitEmptyDirective: PackratParser[PermitEmpty] = "PermitEmptyDirective" ::= directivePrefix ~> "permitEmpty" ^^^ PermitEmpty()

  /**
    * [13] IgnoreColumnNameCaseDirective ::= DirectivePrefix "ignoreColumnNameCase"
    */
  lazy val ignoreColumnNameCaseDirective: PackratParser[IgnoreColumnNameCase] = "IgnoreColumnNameCaseDirective" ::= directivePrefix ~> "ignoreColumnNameCase" ^^^ IgnoreColumnNameCase()

  /**
    * [14]	Body ::= BodyPart+
    */
  lazy val  body = "Body" ::= rep1(bodyPart) <~ rep(eol)

  /**
    * [15] BodyPart ::= Comment* ColumnDefinition Comment*
    */
  lazy val bodyPart = "BodyPart" ::= (rep(comment) ~> columnDefinition) <~ rep(comment)

  /**
    * [16] Comment ::= SingleLineComment | MultiLineComment
    */
  lazy val comment: PackratParser[Any] = "Comment" ::= singleLineComment | multiLineComment

  /**
    * [17] SingleLineComment ::= "//" NonBreakingChar*
    */
  lazy val singleLineComment: Parser[String] = "SingleLineComment" ::= """//[\S\t ]*(?:\r?\n)?""".r

  /**
    * [18] MultiLineComment ::= "/*" Char* "*/"
    */
  lazy val multiLineComment: Parser[String] = "MultiLineComment" ::= """\/\*(?:[^*\r\n]+|(?:\r?\n))*\*\/(?:\r?\n)?""".r

  /**
    * [19] ColumnDefinition ::=  (ColumnIdentifier | QuotedColumnIdentifier) ":" ColumnRule
    */
  lazy val columnDefinition: PackratParser[ColumnDefinition] = "ColumnDefinition" ::= positioned((
    ((columnIdentifier | quotedColumnIdentifier) <~ ":") ~ columnRule <~ (endOfColumnDefinition | comment) ^^ {
      case id ~ (rules ~ columnDirectives) =>
        ColumnDefinition(id, rules, columnDirectives)
    }
    ).withFailureMessage("Invalid column definition"))

  /**
    * [20] ColumnIdentifier ::= PositiveNonZeroIntegerLiteral | Ident
    */
  lazy val columnIdentifier: PackratParser[ColumnIdentifier] = "ColumnIdentifier" ::= (positiveNonZeroIntegerLiteral | ident).withFailureMessage("Column identifier invalid") ^^ {
    case offset: BigInt => OffsetColumnIdentifier(offset)
    case ident: String => NamedColumnIdentifier(ident)
  }

  /**
    * [21] QuotedColumnIdentifier ::= StringLiteral
    */
  lazy val quotedColumnIdentifier = "QuotedColumnIdentifier" ::= stringLiteral.withFailureMessage("Quoted column identifier invalid") ^^ {
    NamedColumnIdentifier(_)
  }

  /**
    * [22] ColumnRule ::= ColumnValidationExpr* ColumnDirectives
    */
  lazy val columnRule = "ColumnRule" ::= rep(columnValidationExpr) ~ columnDirectives

  /**
    * [23] ColumnDirectives ::= OptionalDirective? MatchIsFalseDirective? IgnoreCaseDirective? WarningDirective?    /* expr: unordered */
    */
  lazy val columnDirectives: Parser[List[ColumnDirective]] = "ColumnDirectives" ::= opt(mingle(List(optionalDirective, matchIsFalseDirective, ignoreCaseDirective, warningDirective).map(positioned(_))).withFailureMessage("Invalid column directive")) ^^ {
    _.getOrElse(List.empty)
  }

  /**
    * [24] OptionalDirective ::= DirectivePrefix "optional"
    */
  lazy val optionalDirective = "OptionalDirective" ::= directivePrefix ~> "optional" ^^^ Optional()

  /**
    * [25] MatchIsFalseDirective ::= DirectivePrefix "matchIsFalse"
    */
  //TODO implement workings of matchIsFalseDirective at present it does nothing!
  lazy val matchIsFalseDirective = "MatchIsFalseDirective" ::= directivePrefix ~> "matchIsFalse" ^^^ MatchIsFalse()

  /**
    * [26] IgnoreCaseDirective ::= DirectivePrefix "ignoreCase"
    */
  lazy val ignoreCaseDirective = "IgnoreCaseDirective" ::= directivePrefix ~> "ignoreCase" ^^^ IgnoreCase()

  /**
    * [27] WarningDirective ::=  DirectivePrefix "warningDirective"
    */
  lazy val warningDirective = "WarningDirective" ::= directivePrefix ~> "warning" ^^^ Warning()

  /**
    * [28] ColumnValidationExpr ::= CombinatorialExpr | NonCombinatorialExpr
    */
  lazy val columnValidationExpr: PackratParser[Rule] = "ColumnValidationExpr" ::= positioned(combinatorialExpr | nonCombinatorialExpr)

  /**
    * [29] CombinatorialExpr ::= OrExpr | AndExpr
    */
  lazy val combinatorialExpr = "CombinatorialExpr" ::= orExpr | andExpr

  /**
    * [30] OrExpr  ::= nonCombinatorialExpr "or" columnValidationExpr
    *
    * Uses nonCombinatorialExpr on the left-hand-side
    * to avoid left recursive rule
    */
  lazy val orExpr: PackratParser[OrRule] = "OrExpr" ::= nonCombinatorialExpr ~ "or" ~ columnValidationExpr  ^^ {
    case lhs ~ "or" ~ rhs => OrRule(lhs, rhs)
  }

  /**
    * [31] AndExpr ::= nonCombinatorialExpr "and" ColumnValidationExpr
    *
    * Uses nonCombinatorialExpr on the left-hand-side
    * to avoid left recursive rule
    */
  lazy val andExpr: PackratParser[AndRule] = "AndExpr" ::= nonCombinatorialExpr ~ "and" ~ columnValidationExpr ^^  {
    case lhs ~ "and" ~ rhs =>  AndRule(lhs, rhs)
  }

  /**
    * [32] NonCombinatorialExpr ::= NonConditionalExpr | ConditionalExpr
    */
  lazy val nonCombinatorialExpr = "NonCombinatorialExpr" ::= nonConditionalExpr | conditionalExpr

  /**
    * [33] NonConditionalExpr ::=  SingleExpr | ExternalSingleExpr | ParenthesizedExpr
    */
  lazy val nonConditionalExpr: PackratParser[Rule] = "NonConditionalExpr" ::= singleExpr | externalSingleExpr | parenthesizedExpr

  /**
    * [34] SingleExpr ::=  ExplicitContextExpr? (IsExpr | NotExpr | InExpr |
    *                        StartsWithExpr | EndsWithExpr | RegExpExpr |
    *                        RangeExpr | LengthExpr |
    *                        EmptyExpr | NotEmptyExpr | UniqueExpr | identicalExpr |
    *                        UriExpr |
    *                        XsdDateTimeExpr | XsdDateExpr | XsdTimeExpr |
    *                        UkDateExpr | DateExpr | PartialUkDateExpr | PartialDateExpr |
    *                        uuid4Expr |
    *                        PositiveIntegerExpr | UpperCaseExpr | LowerCaseExpr)
    */
  //TODO need to implement and add DateExpr, PartialDateExpr
  lazy val singleExpr: PackratParser[Rule] = "SingleExpr" ::=
    opt(explicitContextExpr) ~ (isExpr  | notExpr | inExpr |
      startsWithExpr | endsWithExpr | regExpExpr |
      rangeExpr | lengthExpr |
      emptyExpr | notEmptyExpr | uniqueExpr |
      uriExpr | xsdDateTimeExpr  | xsdDateExpr | xsdTimeExpr |
      ukDateExpr | partialUkDateExpr |
      uuid4Expr |
      positiveIntegerExpr ) ^^ {
      case explicitContext ~ rule =>
        rule.explicitColumn = explicitContext
        rule
    }

  /**
    * [35] ExplicitContextExpr ::= ColumnRef "/"
    */
  lazy val explicitContextExpr = "ExplicitContextExpr" ::= columnRef <~ "/"

  /**
    * [36] ColumnRef ::= "$" (ColumnIdentifier | QuotedColumnIdentifier)
    */
  lazy val columnRef: PackratParser[ColumnReference] = "ColumnRef" ::= "$" ~> (columnIdentifier | quotedColumnIdentifier) ^^ {
    ColumnReference(_)
  }

  /**
    * [37] IsExpr ::= "is(" StringProvider ")"
    */
  lazy val isExpr: PackratParser[IsRule] = "IsExpr" ::= "is(" ~> stringProvider <~ ")" ^^ {
    IsRule
  }


  /**
    * [38] NotExpr ::= "not(" StringProvider ")"
    */
  lazy val notExpr: PackratParser[NotRule] = "NotExpr" ::= "not(" ~> stringProvider <~ ")" ^^ {
    NotRule
  }

  /**
    * [39] InExpr ::=  "in(" StringProvider ")"
    */
  lazy val inExpr: PackratParser[InRule] = "InExpr" ::= "in(" ~> stringProvider <~ ")" ^^ {
    InRule
  }

  /**
    * [40] StartsWithExpr ::= "starts(" StringProvider ")"
    */
  lazy val startsWithExpr: PackratParser[StartsRule] = "StartsWithExpr" ::= "starts(" ~> stringProvider <~ ")" ^^ {
    StartsRule
  }

  /**
    * [41] EndsWithExpr ::=  "ends(" StringProvider ")"
    */
  lazy val endsWithExpr: PackratParser[EndsRule] = "EndsWithExpr" ::= "ends(" ~> stringProvider <~ ")" ^^ {
    EndsRule
  }

  /**
    * [42] RegExpExpr  ::= "regex(" StringLiteral ")"
    */
  //TODO could improve error or regex?
  //TODO How to escape quotes inside regex?
  lazy val regExpExpr: PackratParser[RegExpRule] = "RegExpExpr" ::= "regex" ~> """([(]")(.*?)("[)])""".r ^^ {
    case s =>
      RegExpRule(s.dropRight(2).drop(2))
  } withFailureMessage("""regex not correctly delimited as ("your regex")""")

  /**
    * [43] RangeExpr ::= "range(" NumericOrAny "," NumericOrAny ")" /* range is inclusive */
    */

  lazy val  rangeExpr: PackratParser[Rule] = "RangeExpr" ::= "range(" ~> numericLiteral ~ "," ~ numericLiteral <~ ")"  ^^ {
    case a ~ "," ~ b =>
      RangeRule(a, b)
  }



  /**
    * [44] LengthExpr  ::= "length(" (PositiveIntegerOrAny ",")? PositiveIntegerOrAny ")"
    *
    * /*
    * length has 4 forms.
    * 1) length(n) ensures the value is: the exact length n (absolute length)
    * 2) length(n, *) ensures the value is: longer than or equal to n (minimum length)
    * 3) length(*, n) ensures the value is: shorter than or equal to n (maximum length)
    * 4) length(n1, n2) ensures the value is: longer than or equal to n1 AND shorter than or equal to n2 (minumum and maximum lengths)
    * */
    */
  lazy val lengthExpr: PackratParser[LengthRule] = "LengthExpr" ::= "length(" ~> opt(positiveIntegerOrAny <~ ",") ~ positiveIntegerOrAny <~ ")" ^^ {
    case from ~ to =>
      def str(x: Option[BigInt]): String = x.map(_.toString).getOrElse(wildcard)
      LengthRule(from.map(str), str(to))
  }

  /**
    * [45] PositiveIntegerOrAny ::=  PositiveIntegerLiteral | WildcardLiteral
    */
  def positiveIntegerOrAny: Parser[Option[BigInt]] = "PositiveIntegerOrAny" ::= (positiveIntegerLiteral | wildcardLiteral) ^^ {
    case positiveInteger: BigInt =>
      Option(positiveInteger)
    case wildcard =>
      Option.empty
  }

  /**
    * [46] EmptyExpr ::= "empty"
    */
  lazy val emptyExpr = "EmptyExpr" ::= "empty" ^^^ EmptyRule()

  /**
    * [47] NotEmptyExpr ::=  "notEmpty"
    */
  lazy val notEmptyExpr = "NotEmptyExpr" ::= "notEmpty" ^^^ NotEmptyRule()



  /**
    * [48] UniqueExpr ::=  "unique" ("(" ColumnRef ("," ColumnRef)* ")")?
    */
  lazy val uniqueExpr: PackratParser[Rule] = "UniqueExpr" ::= "unique" ~> opt("(" ~> columnRef ~ rep("," ~> columnRef) <~ ")") ^^ {
    case None =>
      UniqueRule()
    case Some((columnRef1 ~ columnRefN)) =>
      UniqueMultiRule(columnRef1 :: columnRefN)
  }

  /**
    * [49] UriExpr ::= "uri"
    */
  lazy val uriExpr = "UriExpr" ::= "uri" ^^^ UriRule()

  /**
    * [50] XsdDateTimeExpr ::= "xDateTime" ("(" XsdDateTimeLiteral "," XsdDateTimeLiteral ")")?
    */
  lazy val xsdDateTimeExpr = "XsdDateTimeExpr" ::= "xDateTime" ~> opt((("(" ~> xsdDateTimeLiteral) <~ ",") ~ (xsdDateTimeLiteral <~ ")")) ^^ {
    case None =>
      XsdDateTimeRule()
    case Some(from ~ to) =>
      XsdDateTimeRangeRule(from, to)
  }


  /**
    * [51] XsdDateExpr ::= "xDate" ("(" XsdDateLiteral "," XsdDateLiteral ")")?
    */
  lazy val xsdDateExpr = "XsdDateExpr" ::= "xDate" ~> opt((("(" ~> xsdDateLiteral) <~ ",") ~ (xsdDateLiteral <~ ")")) ^^ {
    case None =>
      XsdDateRule()
    case Some((from ~ to)) =>
      XsdDateRangeRule(from, to)
  }

  /**
    * [52] XsdTimeExpr ::= "xTime" ("(" XsdTimeLiteral "," XsdTimeLiteral ")")?
    */
  lazy val xsdTimeExpr = "XsdTimeExpr" ::= "xTime" ~> opt((("(" ~> xsdTimeLiteral) <~ ",") ~ (xsdTimeLiteral <~ ")")) ^^ {
    case None =>
      XsdTimeRule()
    case Some((from ~ to)) =>
      XsdTimeRangeRule(from, to)
  }

  /**
    * [53] UkDateExpr ::=  "ukDate" ("(" UkDateLiteral "," UkDateLiteral ")")?
    */
  lazy val ukDateExpr = "UkDateExpr" ::= "ukDate" ~> opt((("(" ~> ukDateLiteral) <~ ",") ~ (ukDateLiteral <~ ")")) ^^ {
    case None =>
      UkDateRule()
    case Some((from ~ to)) =>
      UkDateRangeRule(from, to)
  }

  /**
    * [54] DateExpr ::=  "date(" StringProvider "," StringProvider "," StringProvider ("," XsdDateLiteral "," XsdDateLiteral)? ")"
    */
  //TODO implement DateExpr

  /**
    * [55] PartialUkDateExpr ::= "partUkDate"
    */
  lazy val partialUkDateExpr: PackratParser[PartUkDateRule] = "PartialUkDateExpr" ::= "partUkDate" ^^^ PartUkDateRule()

  /**
    * [56] PartialDateExpr ::= "partDate(" StringProvider "," StringProvider "," StringProvider ")"
    */
  //TODO implement PartialDateExpr

  /**
    * [57] Uuid4Expr ::= "uuid4"
    */
  lazy val uuid4Expr: PackratParser[Uuid4Rule] = "Uuid4Expr" ::= "uuid4" ^^^ Uuid4Rule()

  /**
    * [58] PositiveIntegerExpr ::= "positiveInteger"
    */
  lazy val positiveIntegerExpr: PackratParser[PositiveIntegerRule] = "PositiveIntegerExpr" ::= "positiveInteger" ^^^ PositiveIntegerRule()




  /**
    * [59] StringProvider ::= ColumnRef | StringLiteral
    */
  lazy val stringProvider: PackratParser[ArgProvider] = "StringProvider" ::= columnRef | stringLiteral ^^ {
    s => Literal(Some(s))
  }

  /**
    * [60] ExternalSingleExpr ::= ExplicitContextExpr? (FileExistsExpr | ChecksumExpr | FileCountExpr)
    */
  lazy val externalSingleExpr: PackratParser[Rule] = "ExternalSingleExpr" ::= opt(explicitContextExpr) ~ (fileExistsExpr | checksumExpr | fileCountExpr) ^^ {
    case explicitContext ~ rule =>
      rule.explicitColumn = explicitContext
      rule
  }

  /**
    * [61] FileExistsExpr ::=  "fileExists" ("(" StringProvider ")")? /* optional path to prepend to this cell with filename in */
    */
  lazy val fileExistsExpr: PackratParser[FileExistsRule] = "FileExistsExpr" ::= ("fileExists" ~> opt("(" ~> stringProvider <~ ")")).withFailureMessage("Invalid fileExists rule") ^^ {
    case None =>
      FileExistsRule(pathSubstitutions, enforceCaseSensitivePathChecks)
    case Some(s) =>
      FileExistsRule(pathSubstitutions, enforceCaseSensitivePathChecks, s)
  }


  /**
    * [62] ChecksumExpr ::= "checksum(" FileExpr "," StringLiteral ")" /* first arg is file expr, second arg is algorithm to use for checksum */
    */
  lazy val checksumExpr = "ChecksumExpr" ::= ("checksum(" ~> fileExpr <~ ",") ~ stringLiteral <~ ")" ^^ {
    case files ~ algorithm =>
      ChecksumRule(files._1.getOrElse(Literal(None)), files._2, algorithm, pathSubstitutions, enforceCaseSensitivePathChecks)
  }

  /**
    * [63] FileExpr ::= "file(" (StringProvider ",")? StringProvider ")" /* first (optional) arg is path (or ColumnRef of path) to prepend to second arg, second arg is filename (or ColumnRef of filename) */
    */
  lazy val fileExpr = "FileExpr" ::= "file(" ~> opt(stringProvider <~ ",") ~ stringProvider <~ ")"

  /**
    * [64] FileCountExpr ::= "fileCount(" FileExpr ")"
    */
  lazy val fileCountExpr = "FileCountExpr" ::= "fileCount(" ~> fileExpr <~ ")" ^^ {
    case a  =>
      FileCountRule(a._1.getOrElse(Literal(None)), a._2, pathSubstitutions)
  }

  /**
    * [65] ParenthesizedExpr ::= "(" ColumnValidationExpr+ ")"
    */
  lazy val parenthesizedExpr: PackratParser[ParenthesesRule] = "ParenthesizedExpr" ::= "(" ~> rep1(columnValidationExpr) <~ ")" ^^ {
    ParenthesesRule
  } | failure("unmatched paren")

  /**
    * [66] ConditionalExpr ::= IfExpr
    */
  lazy val conditionalExpr: PackratParser[Rule] = "ConditionalExpr" ::= ifExpr


  /**
    * [67] IfExpr ::= "if(" (CombinatorialExpr | NonConditionalExpr) "," ColumnValidationExpr+ ("," ColumnValidationExpr+)? ")" /* if with optional else */
    */
  lazy val ifExpr: PackratParser[IfRule] = "IfExpr" ::= (("if(" ~> (combinatorialExpr | nonConditionalExpr) <~ ",") ~ rep1(columnValidationExpr) ~ opt("," ~> rep1(columnValidationExpr)) <~ ")" ^^ {
    case condition ~ thenExpr ~ elseExpr =>
      IfRule(condition, thenExpr, elseExpr)
  }) | failure("Invalid rule")


  protected lazy val endOfColumnDefinition: PackratParser[Any] = "endOfColumnDefinition" ::= whiteSpace ~ (eol | endOfInput | failure("Invalid column definition"))

  protected lazy val endOfInput: PackratParser[Any] = "endOfInput" ::= new Parser[Any] {
    def apply(input: Input) = {
      if (input.atEnd) new Success("End of Input reached", input)
      else Failure("End of Input expected", input)
    }
  }
  //</editor-fold>

  //<editor-fold desc="CSV Schema lexical parser combinators">
  /**
    * [68] XsdDateTimeLiteral ::= XsdDateWithoutTimezoneComponent "T" XsdTimeLiteral
    */
  lazy val xsdDateTimeLiteral: Parser[String] = "XsdDateTimeLiteral" ::= (xsdDateWithoutTimezoneComponent + "T" + xsdTimeWithoutTimezoneComponent + xsdOptionalTimezoneComponent).r


  /**
    * [69] XsdDateLiteral ::=  XsdDateWithoutTimezoneComponent XsdTimezoneComponent
    */
  lazy val xsdDateLiteral: Parser[String] = "XsdDateLiteral" ::= (xsdDateWithoutTimezoneComponent + xsdOptionalTimezoneComponent).r

  /**
    * [70] XsdTimeLiteral ::=  XsdTimeWithoutTimezoneComponent XsdTimezoneComponent
    */
  lazy val xsdTimeLiteral: Parser[String] = "XsdTimeLiteral" ::= (xsdTimeWithoutTimezoneComponent + xsdOptionalTimezoneComponent).r

  /**
    * [71] XsdDateWithoutTimezoneComponent ::= -?[0-9]{4}-(((0(1|3|5|7|8)|1(0|2))-(0[1-9]|(1|2)[0-9]|3[0-1]))|((0(4|6|9)|11)-(0[1-9]|(1|2)[0-9]|30))|(02-(0[1-9]|(1|2)[0-9])))    /* xgc:regular-expression */
    */
  //NOTE - we use a more relaxed regexp here than the spec, as another validation parse is done by the relevant rule class (e.g. schema.XsdTimeRegex, schema.XsdDateRegex or schema.XsdDateTimeRegex)
  lazy val xsdDateWithoutTimezoneComponent = "[0-9]{4}-[0-9]{2}-[0-9]{2}"

  /**
    * [72] XsdTimeWithoutTimezoneComponent ::= ([0-1][0-9]|2[0-4]):(0[0-9]|[1-5][0-9]):(0[0-9]|[1-5][0-9])(\.[0-9]{3})?    /* xgc:regular-expression */
    */
  //NOTE - we use a more relaxed regexp here than the spec, as another validation parse is done by the relevant rule class (e.g. schema.XsdTimeRegex, schema.XsdDateRegex or schema.XsdDateTimeRegex)
  lazy val xsdTimeWithoutTimezoneComponent = """[0-9]{2}:[0-9]{2}:[0-9]{2}(\.[0-9]{3})?"""

  /**
    * [73] XsdTimezoneComponent ::=  ((\+|-)(0[1-9]|1[0-9]|2[0-4]):(0[0-9]|[1-5][0-9])|Z)?    /* xgc:regular-expression */
    */
  //NOTE - we use a more relaxed regexp here than the spec, as another validation parse is done by the relevant rule class (e.g. schema.XsdTimeRegex, schema.XsdDateRegex or schema.XsdDateTimeRegex)
  lazy val xsdOptionalTimezoneComponent = "(([+-][0-9]{2}:[0-9]{2})|Z)?"


  /**
    * [74] UkDateLiteral ::= (((0[1-9]|(1|2)[0-9]|3[0-1])\/(0(1|3|5|7|8)|1(0|2)))|((0[1-9]|(1|2)[0-9]|30)\/(0(4|6|9)|11))|((0[1-9]|(1|2)[0-9])\/02))\/[0-9]{4}
    */
  //NOTE - we use a more relaxed parser here than the spec, as another validation parse is done by schema.UkDateRegex in the appropriate rule
  lazy val ukDateLiteral: Parser[String] = "UkDateLiteral" ::= "[0-9]{1,2}/[0-9]{1,2}/[0-9]{4}".r

  /**
    * [75] PositiveNonZeroIntegerLiteral ::= [1-9][0-9]*     /* xgc:regular-expression */    /* A Natural Number, positive integer */
    */
  lazy val positiveNonZeroIntegerLiteral: Parser[BigInt] = "PositiveNonZeroIntegerLiteral" ::= "[1-9][0-9]*".r ^^ {
    BigInt(_)
  }

  /**
    * [76] PositiveIntegerLiteral ::= [0-9]+     /* xgc:regular-expression */    /* A Natural Number, non-negative integer */
    */
  lazy val positiveIntegerLiteral: Parser[BigInt] = "PositiveIntegerLiteral" ::= "[0-9]+".r ^^ {
    BigInt(_)
  }

  /**
    * [77] NumericLiteral ::= -?[0-9]+(\.[0-9]+)?         /* xgc:regular-expression */    /* A Real Number, expressed as an integer or decimal */
    */
  lazy val numericLiteral: Parser[BigDecimal] = "NumericLiteral" ::= """-?[0-9]+(\.[0-9]+)?""".r ^^ {
    BigDecimal(_)
  }

  /**
    * [78] StringLiteral ::= "\"" [^"]* "\""    /* xgc:regular-expression */    /* Any characters except: quotation mark */
    */
  lazy val stringLiteral: Parser[String] = "StringLiteral" ::= "\"" ~> """[^"]*""".r <~ "\""


  /**
    * [79] CharacterLiteral ::= "'" [^\r\n\f'] "'"     /* xgc:regular-expression */    /* Any characters except: carriage-return, line-break, form-feed and apostrophe */
    */
  lazy val characterLiteral: Parser[Char] = "CharacterLiteral" ::= "'" ~> """[^\r\n\f']""".r <~ "'" ^^ {
    _.head
  }

  /**
    * [80] WildcardLiteral ::= "*"
    */
  def wildcardLiteral : Parser[String] = "WildcardLiteral" ::= wildcard

  /**
    * [81] Ident ::= [A-Za-z0-9\-_\.]+   /* xgc:regular-expression */
    */
  lazy val ident: Parser[String] = "Ident" ::= """[A-Za-z0-9\-_\.]+""".r

  protected val wildcard = "*"

  override protected val whiteSpace = """[ \t]*""".r

  protected lazy val eol: Parser[String] = "eol" ::= """\r?\n""".r

  //<editor-fold desc="CSV Schema parser combinators">

  /*
  private val charPattern = """([^"\p{Cntrl}]|\\[\\'"bfnrt]|\\u[a-fA-F0-9]{4})"""
  private val stringRegex = """([^"\p{Cntrl}\\]|\\[\\'"bfnrt]|\\u[a-fA-F0-9]{4})*""".r
  private val stringRegex = s"""$charPattern*""".r     //allow un-escaped '\'
  private val charRegex = charPattern.r
  */
  //</editor-fold>

}