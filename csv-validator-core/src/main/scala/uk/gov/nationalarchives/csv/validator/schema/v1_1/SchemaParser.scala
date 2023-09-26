/*
 * Copyright (c) 2013, The National Archives <digitalpreservation@nationalarchives.gov.uk>
 * https://www.nationalarchives.gov.uk
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package uk.gov.nationalarchives.csv.validator.schema.v1_1

import scala.language.reflectiveCalls
import uk.gov.nationalarchives.csv.validator.schema.v1_0.{SchemaParser => SchemaParser1_0}
import uk.gov.nationalarchives.csv.validator.schema._



/**
  * CSV Schema Parser
  *
  * Uses Scala Parser Combinators to parse the CSV Schema language defined in
  * the specification document
  * Based on the version 1.0
  * @see http://digital-preservation.github.io/csv-validator/csv-schema-1.1.html
  */
trait SchemaParser extends SchemaParser1_0 {

  override lazy val versionDecl: PackratParser[String] = "VersionDecl" ::= ("version" ~> versionLiteral <~ eol).withFailureMessage(s"Schema version declaration 'version ${Schema.version}' missing or incorrect")

  override lazy val rangeExpr: PackratParser[Rule] = "RangeExpr" ::= "range(" ~> numericOrAny ~ "," ~ numericOrAny <~ ")"  ^^ {
    case a ~ "," ~ b => RangeRule(a,b)
  }

  override lazy val conditionalExpr: PackratParser[Rule] = "ConditionalExpr" ::= (ifExpr | switchExpr)


  protected lazy val switchCaseEpr: PackratParser[(Rule,List[Rule])] = "SwitchCaseExpr" ::= (((combinatorialExpr | nonConditionalExpr) <~ ",") ~ rep1(columnValidationExpr) ^^ {
    case condition ~ thenExpr => (condition,thenExpr)
  })

  lazy val switchExpr: PackratParser[SwitchRule] = "SwitchExpr" ::= ( "switch(" ~> repsep( "(" ~> switchCaseEpr <~ ")" , ",") ~ opt("," ~> rep1(columnValidationExpr)) <~ ")" ^^ {
    case switchCaseEpr ~ maybeElseExpr =>
      SwitchRule(maybeElseExpr, switchCaseEpr:_*)
  })

  override lazy val singleExpr: PackratParser[Rule] = "SingleExpr" ::=
    opt(explicitContextExpr) ~ (isExpr | anyExpr | notExpr | inExpr |
      startsWithExpr | endsWithExpr | regExpExpr |
      rangeExpr | lengthExpr |
      emptyExpr | notEmptyExpr | uniqueExpr |
      uriExpr | xsdDateTimeTzExpr | xsdDateTimeExpr  | xsdDateExpr | xsdTimeExpr |
      ukDateExpr | partialUkDateExpr |
      uuid4Expr |
      positiveIntegerExpr | identicalExpr  | upperCaseExpr |
      lowerCaseExpr ) ^^ {
      case explicitContext ~ rule =>
        rule.explicitColumn = explicitContext
        explicitContext.map(rule.explicitColumns += _)
        rule

    }

  /**
    * [37] AnyExpr ::= "any(" (StringProvider)? ")"
    */
  lazy val anyExpr: PackratParser[AnyRule] = "AnyExpr" ::= "any(" ~> rep1sep(stringProvider, ",") <~ ")" ^^ {
    AnyRule
  }

  /**
    * [60] ExternalSingleExpr ::= ExplicitContextExpr? (FileExistsExpr | ChecksumExpr | FileCountExpr)
    */
  override lazy val externalSingleExpr: PackratParser[Rule] = "ExternalSingleExpr" ::= opt(explicitContextExpr) ~ (fileExistsExpr | integrityCheckExpr | checksumExpr | fileCountExpr) ^^ {
    case explicitContext ~ rule =>
      rule.explicitColumn = explicitContext
      explicitContext.map(rule.explicitColumns += _)
      rule
  }

  lazy val integrityCheckExpr: PackratParser[IntegrityCheckRule] = "IntegrityCheckExpr" ::= ("integrityCheck" ~> "(" ~> opt(stringProvider <~ ",") ~ opt(stringLiteral <~ ",") ~ stringLiteral <~ ")"  ).withFailureMessage("Invalid integrityCheck rule") ^^ {
    case rp ~ topLevelFolder ~ includeFolder if (includeFolder == "includeFolder") =>
      IntegrityCheckRule(pathSubstitutions, enforceCaseSensitivePathChecks, rp.getOrElse(Literal(None)), topLevelFolder.getOrElse("content"), true)
    case rp ~ topLevelFolder ~ includeFolder if (includeFolder == "excludeFolder") =>
      IntegrityCheckRule(pathSubstitutions, enforceCaseSensitivePathChecks, rp.getOrElse(Literal(None)), topLevelFolder.getOrElse("content"), false)
  }


  lazy val identicalExpr: PackratParser[IdenticalRule] = "IdenticalExpr" ::= "identical" ^^ {
    case _ => IdenticalRule()
  }

  lazy val upperCaseExpr: PackratParser[UpperCaseRule] = "UpperCaseExpr" ::= "upperCase" ^^ {
    case _ => UpperCaseRule()
  }

  lazy val lowerCaseExpr: PackratParser[LowerCaseRule] = "LowerCaseExpr" ::= "lowerCase" ^^ {
    case _ => LowerCaseRule()
  }

  lazy val xsdDateTimeTzExpr = "XsdDateTimeTzExpr" ::= "xDateTimeTz" ~> opt((("(" ~> xsdDateTimeTzLiteral) <~ ",") ~ (xsdDateTimeTzLiteral <~ ")")) ^^ {
    case None =>
      XsdDateTimeWithTimeZoneRule()
    case Some(from ~ to) =>
      XsdDateTimeWithTimeZoneRangeRule(from, to)
  }

  lazy val xsdDateTimeTzLiteral: Parser[String] = "XsdDateTimeTzLiteral" ::= (xsdDateWithoutTimezoneComponent + "T" + xsdTimeWithoutTimezoneComponent + xsdTimezoneComponent).r

  lazy val xsdTimezoneComponent = "(([+-][0-9]{2}:[0-9]{2})|Z)"

  /**
    * [59] StringProvider ::= ColumnRef | StringLiteral
    */
  override lazy val stringProvider: PackratParser[ArgProvider] = "StringProvider" ::= noExt | concat | columnRef | stringLiteral ^^ {
    s => Literal(Some(s))
  }


  lazy val noExt: PackratParser[ArgProvider] = "NoExt" ::= "noExt(" ~> stringProvider <~ ")" ^^ {
     NoExt(_)
  }

  lazy val concat: PackratParser[ArgProvider] = "Concat" ::= "concat(" ~> rep1sep(stringProvider,",") <~")" ^^ { Concat(_:_*) }




  /**
    * [45.1] NumericOrAny ::=  NumericLiteral | WildcardLiteral
    */
  def numericOrAny: Parser[Option[BigDecimal]] = "NumericOrAny" ::= (numericLiteral | wildcardLiteral) ^^ {
    case positiveInteger: BigDecimal =>
      Option(positiveInteger)
    case wildcard =>
      Option.empty
  }

}
