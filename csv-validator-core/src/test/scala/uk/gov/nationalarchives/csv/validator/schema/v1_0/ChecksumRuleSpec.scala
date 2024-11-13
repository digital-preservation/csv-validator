/*
 * Copyright (c) 2013, The National Archives <digitalpreservation@nationalarchives.gov.uk>
 * https://www.nationalarchives.gov.uk
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package uk.gov.nationalarchives.csv.validator.schema.v1_0

import org.specs2.mutable.Specification
import uk.gov.nationalarchives.csv.validator.{FILE_SEPARATOR, TestResources}
import uk.gov.nationalarchives.csv.validator.api.CsvValidator.SubstitutePath
import uk.gov.nationalarchives.csv.validator.metadata.{Cell, Row}
import uk.gov.nationalarchives.csv.validator.schema._

import cats.data.Validated

class ChecksumRuleSpec extends Specification with TestResources {

  override val checksumPath = resourcePath("checksum.csvs")

  val emptyPathSubstitutions = List[SubstitutePath]()

  "Checksum" should  {

    "fail when calculated algorithm does not match given string value" in {
      val checksumRule = new ChecksumRule(Literal(Some(checksumPath)), "MD5", false)

      checksumRule.evaluate(0, Row(List(Cell("699d61aff25f16a5560372e610da91ab")), 1), Schema(List(TotalColumns(1), NoHeader()), List(ColumnDefinition(NamedColumnIdentifier("column1"))))) must beLike {
        case Validated.Invalid(m) => m.toList mustEqual List("""checksum(file("""" + checksumPath + """"), "MD5") file """" + checksumPath + """" checksum match fails for line: 1, column: column1, value: "699d61aff25f16a5560372e610da91ab". Computed checksum value:"232762380299115da6995e4c4ac22fa2"""")
      }
    }

    "succeed when calculated algorithm does match given string value" in {
      val checksumRule = new ChecksumRule(Literal(Some(checksumPath)), "MD5", false)

      checksumRule.evaluate(0, Row(List(Cell("232762380299115da6995e4c4ac22fa2")), 1), Schema(List(TotalColumns(1), NoHeader()), List(ColumnDefinition(NamedColumnIdentifier("column1"))))) mustEqual Validated.Valid(true)
    }

    "succeed when calculated algorithm does match given string value - without /" in {
      val checksumRule = ChecksumRule(Literal(Some(baseResourcePkgPath)), Literal(Some("""checksum.csvs""")), "MD5", emptyPathSubstitutions, false)

      checksumRule.evaluate(0, Row(List(Cell("232762380299115da6995e4c4ac22fa2")), 1), Schema(List(TotalColumns(1), NoHeader()), List(ColumnDefinition(NamedColumnIdentifier("column1"))))) mustEqual Validated.Valid(true)
    }

    "succeed when calculated algorithm does match given string value - with /" in {
      val checksumRule = ChecksumRule(Literal(Some(baseResourcePkgPath + FILE_SEPARATOR)), Literal(Some("""checksum.csvs""")), "MD5", emptyPathSubstitutions, false)

      checksumRule.evaluate(0, Row(List(Cell("232762380299115da6995e4c4ac22fa2")), 1), Schema(List(TotalColumns(1), NoHeader()), List(ColumnDefinition(NamedColumnIdentifier("column1"))))) mustEqual Validated.Valid(true)
    }
  }

  "checksum with path substitutions" should {
    "succeed with help from substitutions to fix path" in {
      val pathSubstitutions =  List[(String,String)](
        ("bob", relBasePath)
      )

      val checksumRule = new ChecksumRule(Literal(Some("""bob/uk/gov/nationalarchives/csv/validator/schema/v1_0/checksum.csvs""")), "MD5", pathSubstitutions, false)
      checksumRule.evaluate(0, Row(List(Cell("232762380299115da6995e4c4ac22fa2")), 1), Schema(List(TotalColumns(1), NoHeader()), List(ColumnDefinition(NamedColumnIdentifier("column1"))))) mustEqual Validated.Valid(true)
    }

    "succeed with help from substitutions with extra '/'" in {
      val pathSubstitutions =  List[(String,String)](
        ("bob", relBasePath + FILE_SEPARATOR)
      )

      val checksumRule = new ChecksumRule(Literal(Some("""bob/uk/gov/nationalarchives/csv/validator/schema/v1_0/checksum.csvs""")), "MD5", pathSubstitutions, false)
      checksumRule.evaluate(0, Row(List(Cell("232762380299115da6995e4c4ac22fa2")), 1), Schema(List(TotalColumns(1), NoHeader()), List(ColumnDefinition(NamedColumnIdentifier("column1"))))) mustEqual Validated.Valid(true)
    }

    "succeed with help from substitutions with windows path seperators" in {
      val pathSubstitutions =  List[(String,String)](
        ("bob", relBasePath.replace('/', '\\'))
      )

      val checksumRule = new ChecksumRule(Literal(Some("bob\\uk\\gov\\nationalarchives\\csv\\validator\\schema\\v1_0\\checksum.csvs")), "MD5", pathSubstitutions, false)
      checksumRule.evaluate(0, Row(List(Cell("232762380299115da6995e4c4ac22fa2")), 1), Schema(List(TotalColumns(1), NoHeader()), List(ColumnDefinition(NamedColumnIdentifier("column1"))))) mustEqual Validated.Valid(true)
    }

    "succeed when substitutions is not at the start of the path" in {
      val pathSubstitutions =  List[(String,String)](
        ("bob", basePath)
      )

      val checksumRule = new ChecksumRule(Literal(Some("""file:///bob/uk/gov/nationalarchives/csv/validator/schema/v1_0/checksum.csvs""")), "MD5", pathSubstitutions, false)
      checksumRule.evaluate(0, Row(List(Cell("232762380299115da6995e4c4ac22fa2")), 1), Schema(List(TotalColumns(1), NoHeader()), List(ColumnDefinition(NamedColumnIdentifier("column1"))))) mustEqual Validated.Valid(true)
    }

  }
}
