/*
 * Copyright (c) 2013, The National Archives <digitalpreservation@nationalarchives.gov.uk>
 * https://www.nationalarchives.gov.uk
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package uk.gov.nationalarchives.csv.validator.schema.v1_1

import org.junit.runner.RunWith
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner
import uk.gov.nationalarchives.csv.validator._
import uk.gov.nationalarchives.csv.validator.api.CsvValidator.SubstitutePath
import uk.gov.nationalarchives.csv.validator.metadata.{Cell, Row}
import uk.gov.nationalarchives.csv.validator.schema._

import cats.data.Validated

@RunWith(classOf[JUnitRunner])
class IntegrityCheckRuleSpec extends Specification with TestResources {

//  val relMustExistForRulePath = relResourcePath("mustExistForRule.csvs")
  val relIntegrityCheckForRulePath = relResourcePath("integrityCheck" + FILE_SEPARATOR + "folder1" + FILE_SEPARATOR + "content" + FILE_SEPARATOR + "file1.txt")
  val relIntegrityCheckForRulePath2 = relResourcePath("integrityCheck" + FILE_SEPARATOR + "folder1" + FILE_SEPARATOR + "content" + FILE_SEPARATOR + "file#2.txt")

//  val relPath = relativePath(relMustExistForRulePath)
  
  val relMustExistForHashRulePath = relResourcePath("mustExistFor#Rule.csvs")
  val hashSegments = relMustExistForHashRulePath.split(FILE_SEPARATOR)
  val hashRelPath = (hashSegments.slice(0, hashSegments.length - 3).mkString(FILE_SEPARATOR.toString), hashSegments.slice(hashSegments.length - 3, hashSegments.length).mkString(FILE_SEPARATOR.toString))


  val relPath2 = relativePath(relIntegrityCheckForRulePath)
  val relPath3 = relativePath(relIntegrityCheckForRulePath2)

  def relativePath(path: String):(String,String) = {
    val segments = path.split(FILE_SEPARATOR)
    (segments.slice(0, segments.length - 3).mkString(FILE_SEPARATOR.toString), segments.slice(segments.length - 3, segments.length).mkString(FILE_SEPARATOR.toString))
  }
  val emptyPathSubstitutions = List[SubstitutePath]()



  "IntegrityCheckRule" should {

    val globalDirsOne = List(TotalColumns(1))
    val globalDirsTwo = List(TotalColumns(2))

    "fail for extra file" in {
      val integrityCheckRule = IntegrityCheckRule(emptyPathSubstitutions,false)

      val schema: Schema = Schema(globalDirsOne, List(ColumnDefinition(NamedColumnIdentifier("column1"))))
      val totalRows: Some[Boolean] = Some(false)
      
      integrityCheckRule.evaluate(0, Row(List(Cell(relIntegrityCheckForRulePath)), 1), schema, totalRows) must beLike {
        case Validated.Invalid(messages) => messages.head must be matching "integrityCheck fails for line: 1, column: column1, files: \"[a-z0-9._\\\\/-]*integrityCheck[\\\\/]folder1[\\\\/]content[\\\\/]file#2.txt\" are not listed in the metadata"
      }
    }

    "fail for empty file path" in {
      val integrityCheckRule = IntegrityCheckRule(emptyPathSubstitutions,false)
      val schema: Schema = Schema(globalDirsTwo, List(ColumnDefinition(NamedColumnIdentifier("column1")), ColumnDefinition(NamedColumnIdentifier("column2"))))

      integrityCheckRule.evaluate(1, Row(List(Cell("abc"), Cell(relIntegrityCheckForRulePath)), 1), schema, Some(true)) mustEqual Validated.Valid(true)

      integrityCheckRule.evaluate(1, Row(List(Cell("abc"), Cell("")), 2), schema, Some(false)) must beLike {
        case Validated.Invalid(messages) => messages.head must be matching "integrityCheck fails for line: 2, column: column2, files: \"[a-z0-9._\\\\/-]*integrityCheck[\\\\/]folder1[\\\\/]content[\\\\/]file#2.txt\" are not listed in the metadata"
      }
    }

    "succeed for file that exists with no root file path" in {

      val integrityCheckRule = IntegrityCheckRule(emptyPathSubstitutions,false)

      val schema: Schema = Schema(globalDirsOne, List(ColumnDefinition(NamedColumnIdentifier("column1"))))

      integrityCheckRule.evaluate(0, Row(List(Cell(relIntegrityCheckForRulePath)), 1), schema, Some(true)) mustEqual Validated.Valid(true)
      integrityCheckRule.evaluate(0, Row(List(Cell(relIntegrityCheckForRulePath2)), 2), schema, Some(false)) mustEqual Validated.Valid(true)

    }

   "succeed for file that exists with root file path" in {
     val integrityCheckRule = IntegrityCheckRule(emptyPathSubstitutions, false, Literal(Some(relPath2._1 + FILE_SEPARATOR)))

     val schema: Schema = Schema(globalDirsOne, List(ColumnDefinition(NamedColumnIdentifier("column1"))))


    integrityCheckRule.evaluate(0, Row(List(Cell(relPath2._2)), 1), schema, Some(true)) must be_==(Validated.Valid(true))
    integrityCheckRule.evaluate(0, Row(List(Cell(relPath3._2)), 2), schema, Some(false)) must be_==(Validated.Valid(true))

  }

   "succeed for root file path without final file separator and file without initial file separator" in {

     val integrityCheckRule = IntegrityCheckRule(emptyPathSubstitutions, false, Literal(Some(relPath2._1)))

     val schema: Schema = Schema(globalDirsOne, List(ColumnDefinition(NamedColumnIdentifier("column1"))))

     integrityCheckRule.evaluate(0, Row(List(Cell(relPath2._2)), 1), schema, Some(true)) must be_==(Validated.Valid(true))
     integrityCheckRule.evaluate(0, Row(List(Cell(relPath3._2)), 2), schema, Some(false)) must be_==(Validated.Valid(true))

   }

  }

}
