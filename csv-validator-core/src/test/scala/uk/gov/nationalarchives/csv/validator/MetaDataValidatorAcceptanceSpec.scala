/**
 * Copyright (c) 2013, The National Archives <digitalpreservation@nationalarchives.gov.uk>
 * http://www.nationalarchives.gov.uk
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package uk.gov.nationalarchives.csv.validator

import org.specs2.mutable.Specification
import scalaz._
import uk.gov.nationalarchives.csv.validator.schema.Schema
import uk.gov.nationalarchives.csv.validator.api.CsvValidator
import scalax.file.Path
import java.io.StringReader
import java.io


class MetaDataValidatorAcceptanceSpec extends Specification with TestResources {

  val base = acceptancePath

  val v = new CsvValidator with AllErrorsMetaDataValidator {
    val pathSubstitutions = List[(String,String)]()

    def validateR(csv: io.Reader, schema: Schema): this.type#MetaDataValidation[Any] = validate(csv, schema)
  }

  import v.{validate, validateR, parseSchema}

  def parse(filePath: String): Schema = parseSchema(Path.fromString(filePath)) fold (f => throw new IllegalArgumentException(f.toString()), s => s)

  def parse(reader: io.Reader): Schema = parseSchema(reader) fold (f => throw new IllegalArgumentException(f.toString()), s => s)

  "Regex rule" should {

    "succeed for metadata file with column that passes regex rule" in {
      validate(Path.fromString(base) / "regexRulePassMetaData.csv", parse(base + "/regexRuleSchema.txt")) must beLike {
        case Success(_) => ok
      }
    }

    "succeed when @noHeader not set as first line is skipped" in {

      validate(Path.fromString(base) / "regexRuleFailMetaData.csv", parse(base + "/regexRuleSchema.txt")) must beLike {
        case Success(_) => ok
      }
    }

    "succeed when @noHeader not set" in {
      validate(Path.fromString(base) / "regexRuleFailMetaData.csv", parse(base + "/regexRuleSchemaWithoutNoHeaderSet.txt")) must beLike {
        case Success(_) => ok
      }
    }
  }

  "Multiple errors " should {
    "all be reported" in {
      validate(Path.fromString(base) / "multipleErrorsMetaData.csv", parse(base + "/regexRuleSchemaWithNoHeaderSet.txt")) must beLike {
        case Failure(errors) => errors.list mustEqual List(
          ErrorMessage("""regex("[0-9]+") fails for line: 1, column: Age, value: "twenty""""),
          ErrorMessage("""regex("[0-9]+") fails for line: 2, column: Age, value: "thirty""""))
      }
    }
  }

  "Combining two rules" should {
    "succeed when metadata valid" in {
      validate(Path.fromString(base) / "twoRulesPassMetaData.csv", parse(base + "/twoRuleSchema.txt")) must beLike {
        case Success(_) => ok
      }
    }

    "fail when rules fail for all permutations" in {
      validate(Path.fromString(base) / "twoRulesFailMetaData.csv", parse(base + "/twoRuleSchemaFail.txt")) must beLike {
        case Failure(errors) => errors.list mustEqual List(
          ErrorMessage("""in($FullName) fails for line: 1, column: Name, value: "Ben""""),
          ErrorMessage("""regex("[a-z]+") fails for line: 1, column: Name, value: "Ben""""),
          ErrorMessage("""in($FullName) fails for line: 2, column: Name, value: "Dave""""),
          ErrorMessage("""regex("[a-z]+") fails for line: 2, column: Name, value: "Dave""""))
      }
    }
  }

  "An in rule" should {
    "succeed if the column value is in the rule's literal string" in {
      validate(Path.fromString(base) / "inRulePassMetaData.csv", parse(base + "/inRuleSchema.txt")) must beLike {
        case Success(_) => ok
      }
    }

    "fail if the column value is not in the rule's literal string" in {
      validate(Path.fromString(base) / "inRuleFailMetaData.csv", parse(base + "/inRuleSchema.txt")) must beLike {
        case Failure(errors) => errors.list mustEqual List(
          ErrorMessage("""in("thevaluemustbeinthisstring") fails for line: 1, column: SomeInRule, value: "valuenotinrule""""),
          ErrorMessage("""in("thevaluemustbeinthisstring") fails for line: 3, column: SomeInRule, value: "thisonewillfailtoo""""))
      }
    }

    "succeed if the column value is in the rule's cross referenced column" in {
      validate(Path.fromString(base) / "inRuleCrossReferencePassMetaData.csv", parse(base + "/inRuleCrossReferenceSchema.txt")) must beLike {
        case Success(_) => ok
      }
    }

    "fail if the column value is not in the rule's cross referenced column" in {
      validate(Path.fromString(base) / "inRuleCrossReferenceFailMetaData.csv", parse(base + "/inRuleCrossReferenceSchema.txt")) must beLike {
        case Failure(errors) => errors.list mustEqual List(ErrorMessage("""in($FullName) fails for line: 2, column: FirstName, value: "Dave""""))
      }
    }
  }

  "An @optional column directive" should {
    "allow a column to have an empty value and ignore other rules" in {
      validate(Path.fromString(base) / "optionalPassMetaData.csv", parse(base + "/optionalSchema.txt")) must beLike {
        case Success(_) => ok
      }
    }

    "fail if a non empty value fails a rule" in {
      validate(Path.fromString(base) / "optionalFailMetaData.csv", parse(base + "/optionalSchema.txt")) must beLike {
        case Failure(errors) => errors.list mustEqual List(ErrorMessage("in($FullName) fails for line: 1, column: Name, value: \"BP\""))
      }
    }
  }

  "An @ignoreCase column directive" should {
    "pass a rule ignoring case" in {
      validate(Path.fromString(base) / "ignoreCasePassMetaData.csv", parse(base + "/ignoreCaseSchema.txt")) must beLike {
        case Success(_) => ok
      }
    }
  }

  "A fileExists rule" should {

    val schemaPath = Path.fromString(base) / "fileExistsSchema.txt"
    val schemaTemplate = schemaPath.lines(includeTerminator = true).mkString
    val schema = schemaTemplate.replaceAll("""\$\$acceptancePath\$\$""", base)

    "ensure the file exists on the file system" in {
      validate(Path.fromString(base) / "fileExistsPassMetaData.csv", parse(new StringReader(schema))) must beLike {
        case Success(_) => ok
      }
    }

    "ensure the file exists on the file system" in {

      val csvPath = Path.fromString(base) / "fileExistsCrossRefPassMetaData.csv"
      val csvTemplate = csvPath.lines(includeTerminator = true).mkString
      val csv = csvTemplate.replaceAll("""\$\$acceptancePath\$\$""", base)

      validateR(new StringReader(csv), parse(base + "/fileExistsCrossRefSchema.txt")) must beLike {
        case Success(_) => ok
      }
    }

    "fail if the file does not exist on the file system" in {
      validate(Path.fromString(base) / "fileExistsPassMetaData.csv", parse(base + "/fileExistsSchemaWithBadBasePath.txt")) must beLike {
        case Failure(errors) => errors.list mustEqual List(
          ErrorMessage("""fileExists("src/test/resources/uk/gov/nationalarchives") fails for line: 1, column: PasswordFile, value: "benPass.txt""""),
          ErrorMessage("""fileExists("src/test/resources/uk/gov/nationalarchives") fails for line: 2, column: PasswordFile, value: "andyPass.txt""""))
      }
    }
  }

  "Validate fail fast" should {
    val app = new CsvValidator with FailFastMetaDataValidator  { val pathSubstitutions = List[(String,String)]() }

    "only report first error for invalid @TotalColumns" in {
      app.validate(Path.fromString(base) / "totalColumnsFailMetaData.csv", parse(base + "/totalColumnsSchema.txt")) must beLike {
        case Failure(errors) => errors.list mustEqual List(ErrorMessage("Expected @totalColumns of 1 and found 2 on line 2"))
      }
    }

    "only report first rule fail for multiple rules on a column" in {
      app.validate(Path.fromString(base) / "rulesFailMetaData.csv", parse(base + "/rulesSchema.txt")) must beLike {
        case Failure(errors) => errors.list mustEqual List(ErrorMessage("""regex("[A-Z][a-z]+") fails for line: 2, column: Name, value: "ben""""))
      }
    }

    "succeed for multiple rules with valid metadata" in {
      app.validate(Path.fromString(base) / "twoRulesPassMetaData.csv", parse(base + "/twoRuleSchema.txt")) must beLike {
        case Success(_) => ok
      }
    }
  }

  "validate uk.gov.nationalarchives.csv.validator.schema" should {

    "fail with duplicate column ids" in {
      parseSchema(Path.fromString(base) / "duplicateColumnIdsFailSchema.txt" ) must beLike {
        case Failure(errors) => errors.list mustEqual List(SchemaMessage("""Column: Age has duplicates on lines 3, 8
                                                             |Column: Country has duplicates on lines 4, 5, 7""".stripMargin))
      }
    }

    "succeed with unique column ids" in {
      validate(Path.fromString(base) / "duplicateColumnIdsMetaData.csv", parse(base + "/duplicateColumnIdsPassSchema.txt")) must beLike {
        case Success(_) => ok
      }
    }
  }

  "An 'or' rule" should {

    "succeed if either the lhs or rhs succeeds" in {
      validate(Path.fromString(base) / "orWithTwoRulesPassMetaData.csv", parse(base + "/orWithTwoRulesSchema.txt")) must beLike {
        case Success(_) => ok
      }
    }

    "fail if both the lhs or rhs are fail" in {
      validate(Path.fromString(base) / "orWithTwoRulesFailMetaData.csv", parse(base + "/orWithTwoRulesSchema.txt")) must beLike {
        case Failure(errors) => errors.list mustEqual List(ErrorMessage("""regex("[A-Z][a-z]+") or regex("[0-9]+") fails for line: 4, column: CountryOrCountryCode, value: "Andromeda9""""))
      }
    }

    "succeed for 2 'or' rules with an 'and' rule" in {
      validate(Path.fromString(base) / "orWithFourRulesPassMetaData.csv", parse(base + "/orWithFourRulesSchema.txt")) must beLike {
        case Success(_) => ok
      }
    }

    "fail if 'or' rules pass and 'and' rule fails" in {
      validate(Path.fromString(base) / "orWithFourRulesFailMetaData.csv", parse(base + "/orWithFourRulesSchema.txt")) must beLike {
        case Failure(errors) => errors.list mustEqual List(ErrorMessage("""regex("[A-Z].+") fails for line: 2, column: Country, value: "ngland""""))
      }
    }
  }

  "No arg standard rules" should {
    "succeed if all the rules are valid" in {
      validate(Path.fromString(base) / "standardRulesPassMetaData.csv", parse(base + "/standardRulesSchema.txt")) must beLike {
        case Success(_) => ok
      }
    }

    "fail if all the rules are not" in {
      validate(Path.fromString(base) / "standardRulesFailMetaData.csv", parse(base + "/standardRulesSchema.txt")) must beLike {
        case Failure(errors) => errors.list.toString() mustEqual """List(ErrorMessage(uri fails for line: 1, column: uri, value: "http:##datagov.nationalarchives.gov.uk#66#WO#409#9999#0#aaaaaaaa-aaaa-4aaa-9eee-0123456789ab"), ErrorMessage(xDateTime fails for line: 1, column: xDateTime, value: "2002-999-30T09:00:10"), ErrorMessage(xDate fails for line: 1, column: xDate, value: "02-99-30"), ErrorMessage(ukDate fails for line: 1, column: ukDate, value: "99/00/0009"), ErrorMessage(xTime fails for line: 1, column: xTime, value: "99:00:889"), ErrorMessage(uuid4 fails for line: 1, column: uuid4, value: "aaaaaaaab-aaaab-4aaa-9eee-0123456789ab"), ErrorMessage(positiveInteger fails for line: 1, column: positiveInteger, value: "12-0912459"))"""

      }
    }
  }
}