/**
 * Copyright (c) 2013, The National Archives <digitalpreservation@nationalarchives.gov.uk>
 * http://www.nationalarchives.gov.uk
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package uk.gov.nationalarchives.csv.validator

import org.junit.runner.RunWith
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner
import scalaz._
import uk.gov.nationalarchives.csv.validator.schema.Schema
import uk.gov.nationalarchives.csv.validator.api.{TextFile, CsvValidator}
import scalax.file.Path
import java.io.StringReader
import java.io

@RunWith(classOf[JUnitRunner])
class MetaDataValidatorAcceptanceSpec extends Specification with TestResources {

  val base = acceptancePath

  val v = new CsvValidator with AllErrorsMetaDataValidator {
    val pathSubstitutions = List[(String,String)]()
    val enforceCaseSensitivePathChecks = false
    val trace = false

    def validateR(csv: io.Reader, schema: Schema): this.type#MetaDataValidation[Any] = validate(csv, schema, None)
  }

  val ve = new CsvValidator with AllErrorsMetaDataValidator {
    val pathSubstitutions = List[(String,String)]()
    val enforceCaseSensitivePathChecks = true
    val trace = false
  }

  import v.{validate, validateR, parseSchema}
  import ve.{validate => validateE, parseSchema => parseSchemaE}

  def parse(filePath: String): Schema = parseSchema(TextFile(Path.fromString(filePath))) fold (f => throw new IllegalArgumentException(f.toString()), s => s)
  def parseE(filePath: String): Schema = parseSchemaE(TextFile(Path.fromString(filePath))) fold (f => throw new IllegalArgumentException(f.toString()), s => s)

  def parse(reader: io.Reader): Schema = parseSchema(reader) fold (f => throw new IllegalArgumentException(f.toString()), s => s)
  def parseE(reader: io.Reader): Schema = parseSchemaE(reader) fold (f => throw new IllegalArgumentException(f.toString()), s => s)

  "@separator global directive" should {
    "succeed for '$' separator" in {
      validate(TextFile(Path.fromString(base) / "separated1.dsv"), parse(base + "/separated1.csvs"), None).isSuccess mustEqual true
    }

    "succeed for TAB separator" in {
      validate(TextFile(Path.fromString(base) / "separated2.tsv"), parse(base + "/separated2.csvs"), None).isSuccess mustEqual true
    }

    "succeed for '\t' separator" in {
      validate(TextFile(Path.fromString(base) / "separated2.tsv"), parse(base + "/separated2-1.csvs"), None).isSuccess mustEqual true
    }

    "with @quoted global directive" should {
      "succeed for '$' separator" in {
        validate(TextFile(Path.fromString(base) / "separated3.dsv"), parse(base + "/separated3.csvs"), None).isSuccess mustEqual true
      }

      "succeed for TAB separator" in {
        validate(TextFile(Path.fromString(base) / "separated4.tsv"), parse(base + "/separated4.csvs"), None).isSuccess mustEqual true
      }

      "succeed for '\t' separator" in {
        validate(TextFile(Path.fromString(base) / "separated4.tsv"), parse(base + "/separated4-1.csvs"), None).isSuccess mustEqual true
      }
    }
  }

  "Regex rule" should {

    "succeed for metadata file with column that passes regex rule" in {
      validate(TextFile(Path.fromString(base) / "regexRulePassMetaData.csv"), parse(base + "/regexRuleSchema.csvs"), None).isSuccess mustEqual true
    }

    "fail when @noHeader not set" in {
      validate(TextFile(Path.fromString(base) / "regexRuleFailMetaData.csv"), parse(base + "/regexRuleSchemaWithoutNoHeaderSet.csvs"), None) must beLike {
        case Failure(errors) => errors.list mustEqual IList(
          FailMessage(ValidationError, "regex(\"[0-9]+\") fails for line: 1, column: Age, value: \"twenty\"",Some(1),Some(1))
        )
      }
    }
  }


  "Not empty rule" should {
    "succeed for metadata file with column that passes regex rule" in {
      validate(TextFile(Path.fromString(base) / "notempty.csv"), parse(base + "/notempty.csvs"), None).isSuccess mustEqual true
    }
  }

  "Multiple errors " should {
    "all be reported" in {
      validate(TextFile(Path.fromString(base) / "multipleErrorsMetaData.csv"), parse(base + "/regexRuleSchemaWithNoHeaderSet.csvs"), None) must beLike {
        case Failure(errors) => errors.list mustEqual IList(
          FailMessage(ValidationError, """regex("[0-9]+") fails for line: 1, column: Age, value: "twenty"""",Some(1),Some(1)),
          FailMessage(ValidationError, """regex("[0-9]+") fails for line: 2, column: Age, value: "thirty"""",Some(2),Some(1)))
      }
    }
  }

  "Combining two rules" should {
    "succeed when metadata valid" in {
      validate(TextFile(Path.fromString(base) / "twoRulesPassMetaData.csv"), parse(base + "/twoRuleSchema.csvs"), None).isSuccess mustEqual true
    }

    "fail when rules fail for all permutations" in {
      validate(TextFile(Path.fromString(base) / "twoRulesFailMetaData.csv"), parse(base + "/twoRuleSchemaFail.csvs"), None) must beLike {
        case Failure(errors) => errors.list mustEqual IList(
          FailMessage(ValidationError, """in($FullName) fails for line: 1, column: Name, value: "Ben"""",Some(1),Some(0)),
          FailMessage(ValidationError, """regex("[a-z]+") fails for line: 1, column: Name, value: "Ben"""",Some(1),Some(0)),
          FailMessage(ValidationError, """in($FullName) fails for line: 2, column: Name, value: "Dave"""",Some(2),Some(0)),
          FailMessage(ValidationError, """regex("[a-z]+") fails for line: 2, column: Name, value: "Dave"""",Some(2),Some(0)))
      }
    }
  }

  "An if rule" should {
    "succeed if the conditionExpr and thenExpr are respected" in {
      validate(TextFile(Path.fromString(base) / "ifRulePassMetaData.csv"), parse(base + "/ifRuleSchema.csvs"), None).isSuccess mustEqual  true
    }

    "succeed if the condition and thenExpr or elseExpr are respected" in {
      validate(TextFile(Path.fromString(base) / "ifRulePassMetaData.csv"), parse(base + "/ifElseRuleSchema.csvs"), None).isSuccess mustEqual  true
    }

    "fail if the conditionExpr is true but the thenExpr is false" in {
      validate(TextFile(Path.fromString(base) / "ifRuleFailThenMetaData.csv"), parse(base + "/ifElseRuleSchema.csvs"), None) must beLike {
        case Failure(errors) => errors.list mustEqual IList(
          FailMessage(ValidationError, """is("hello world") fails for line: 1, column: SomeIfRule, value: "hello world1"""",Some(1),Some(1))
        )
      }
    }

    "fail if the conditionExpr is fasle but the elseExpr is false" in {
      validate(TextFile(Path.fromString(base) / "ifRuleFailElseMetaData.csv"), parse(base + "/ifElseRuleSchema.csvs"), None) must beLike {
        case Failure(errors) => errors.list mustEqual IList(
          FailMessage(ValidationError, """upperCase fails for line: 3, column: SomeIfRule, value: "EFQWeGW"""",Some(3),Some(1))
        )
      }
    }
  }

  "An switch rule" should {
    "succeed if the conditionExpr and thenExpr are respected - 1" in {
      validate(TextFile(Path.fromString(base) / "switch1RulePassMetaData.csv"), parse(base + "/switch1RuleSchema.csvs"), None).isSuccess mustEqual  true
    }

    "succeed if the conditionExpr and thenExpr are respected - 2" in {
      validate(TextFile(Path.fromString(base) / "switch2RulePassMetaData.csv"), parse(base + "/switch2RuleSchema.csvs"), None).isSuccess mustEqual  true
    }

    "fail if the conditionExpr is true but the thenExpr is false - 1" in {
      validate(TextFile(Path.fromString(base) / "switch1RuleFailMetaData.csv"), parse(base + "/switch1RuleSchema.csvs"), None) must beLike {
        case Failure(errors) => errors.list mustEqual IList(
          FailMessage(ValidationError, """is("hello world") fails for line: 1, column: SomeSwitchRule, value: "hello world1"""",Some(1),Some(1))
        )
      }
    }

    "fail if the conditionExpr is true but the thenExpr is false - 2" in {
      validate(TextFile(Path.fromString(base) / "switch2RuleFailMetaData.csv"), parse(base + "/switch2RuleSchema.csvs"), None) must beLike {
        case Failure(errors) =>errors.list mustEqual IList(
          FailMessage(ValidationError, """is("hello world") fails for line: 1, column: SomeSwitchRule, value: "hello world1"""",Some(1),Some(1)),
          FailMessage(ValidationError, """is("HELLO WORLD") fails for line: 2, column: SomeSwitchRule, value: "HELLO WORLD1"""",Some(2),Some(1))
        )
      }
    }

  }


  "An in rule" should {
    "succeed if the column value is in the rule's literal string" in {
      validate(TextFile(Path.fromString(base) / "inRulePassMetaData.csv"), parse(base + "/inRuleSchema.csvs"), None).isSuccess mustEqual true
    }

    "fail if the column value is not in the rule's literal string" in {
      validate(TextFile(Path.fromString(base) / "inRuleFailMetaData.csv"), parse(base + "/inRuleSchema.csvs"), None) must beLike {
        case Failure(errors) => errors.list mustEqual IList(
          FailMessage(ValidationError, """in("thevaluemustbeinthisstring") fails for line: 1, column: SomeInRule, value: "valuenotinrule"""",Some(1),Some(1)),
          FailMessage(ValidationError, """in("thevaluemustbeinthisstring") fails for line: 3, column: SomeInRule, value: "thisonewillfailtoo"""",Some(3),Some(1)))
      }
    }

    "succeed if the column value is in the rule's cross referenced column" in {
      validate(TextFile(Path.fromString(base) / "inRuleCrossReferencePassMetaData.csv"), parse(base + "/inRuleCrossReferenceSchema.csvs"), None).isSuccess mustEqual true
    }

    "fail if the column value is not in the rule's cross referenced column" in {
      validate(TextFile(Path.fromString(base) / "inRuleCrossReferenceFailMetaData.csv"), parse(base + "/inRuleCrossReferenceSchema.csvs"), None) must beLike {
        case Failure(errors) => errors.list mustEqual IList(FailMessage(ValidationError, """in($FullName) fails for line: 2, column: FirstName, value: "Dave"""",Some(2),Some(0)))
      }
    }
  }

  "An any rule" should {
    "succeed if the column value is in the rule's literal string" in {
      validate(TextFile(Path.fromString(base) / "anyRulePassMetaData.csv"), parse(base + "/anyRuleSchema.csvs"), None).isSuccess mustEqual true
    }

    "fail if the column value is not in the rule's literal string" in {
      validate(TextFile(Path.fromString(base) / "anyRuleFailMetaData.csv"), parse(base + "/anyRuleSchema.csvs"), None) must beLike {
        case Failure(errors) => errors.list mustEqual IList(
          FailMessage(ValidationError, """any("value1", "value2", "value3") fails for line: 4, column: SomeAnyRule, value: "value4"""",Some(4),Some(1))
        )
      }
    }
  }

  "A xsdDateTime rule" should {
    "succeed if the column value is in the rule's literal string" in {
      validate(TextFile(Path.fromString(base) / "xsdDateTimePass.csv"), parse(base + "/xsdDateTime.csvs"), None).isSuccess mustEqual true
    }

    "fail if the column value is not in the rule's literal string" in {
      validate(TextFile(Path.fromString(base) / "xsdDateTimeFail.csv"), parse(base + "/xsdDateTime.csvs"), None) must beLike {
        case Failure(errors) => errors.list mustEqual IList(
          FailMessage(ValidationError, """xDateTime fails for line: 2, column: date, value: "2013-03-22"""",Some(2),Some(0))
        )
      }
    }
  }

  "A xsdDateTimeRange rule" should {
    "succeed if the column value is in the rule's literal string" in {
      validate(TextFile(Path.fromString(base) / "xsdDateTimeRangePass.csv"), parse(base + "/xsdDateTimeRange.csvs"), None).isSuccess mustEqual true
    }

    "fail if the column value is not in the rule's literal string" in {
      validate(TextFile(Path.fromString(base) / "xsdDateTimeRangeFail.csv"), parse(base + "/xsdDateTimeRange.csvs"), None) must beLike {
        case Failure(errors) => errors.list  mustEqual IList(
          FailMessage(ValidationError, """xDateTime("2012-01-01T01:00:00, 2013-01-01T01:00:00") fails for line: 2, column: date, value: "2014-01-01T01:00:00"""",Some(2),Some(0))
        )
      }
    }
  }

  //FIXME
  "A xsdDateTimeWithTimeZone rule" should {
    "succeed if the column value is in the rule's literal string" in {
      validate(TextFile(Path.fromString(base) / "xsdDateTimeTzPass.csv"), parse(base + "/xsdDateTimeTz.csvs"), None).isSuccess mustEqual true
    }

    "fail if the column value is invalid" in {
      validate(TextFile(Path.fromString(base) / "xsdDateTimeTzFail.csv"), parse(base + "/xsdDateTimeTz.csvs"), None) must beLike {
        case Failure(errors) => errors.list mustEqual IList(
          FailMessage(ValidationError, """xDateTimeWithTimeZone fails for line: 4, column: date, value: "2012-01-01T00:00:00"""",Some(4),Some(0))
        )
      }
    }
  }

  "A xsdDateTimeWithTimeZoneRange rule" should {
    "succeed if the column value is in the rule's literal string" in {
      validate(TextFile(Path.fromString(base) / "xsdDateTimeTzRangePass.csv"), parse(base + "/xsdDateTimeTzRange.csvs"), None).isSuccess mustEqual true
    }

    "fail if the column value is not in the rule's literal string" in {
      validate(TextFile(Path.fromString(base) / "xsdDateTimeTzRangeFail.csv"), parse(base + "/xsdDateTimeTzRange.csvs"), None) must beLike {
        case Failure(errors) => errors.list  mustEqual IList(
          FailMessage(ValidationError, """xDateTimeWithTimeZone("2012-01-01T01:00:00+00:00, 2013-01-01T01:00:00+00:00") fails for line: 2, column: date, value: "2014-01-01T01:00:00+00:00"""",Some(2),Some(0))
        )
      }
    }
  }



  "An @optional column directive" should {
    "allow a column to have an empty value and ignore other rules" in {
      validate(TextFile(Path.fromString(base) / "optionalPassMetaData.csv"), parse(base + "/optionalSchema.csvs"), None).isSuccess mustEqual true
    }

    "fail if a non empty value fails a rule" in {
      validate(TextFile(Path.fromString(base) / "optionalFailMetaData.csv"), parse(base + "/optionalSchema.csvs"), None) must beLike {
        case Failure(errors) => errors.list mustEqual IList(FailMessage(ValidationError, "in($FullName) fails for line: 1, column: Name, value: \"BP\"",Some(1),Some(0)))
      }
    }
  }

  "An @ignoreCase column directive" should {
    "pass a rule ignoring case" in {
      validate(TextFile(Path.fromString(base) / "ignoreCasePassMetaData.csv"), parse(base + "/ignoreCaseSchema.csvs"), None).isSuccess mustEqual true
    }
  }

  "A fileExists rule" should {

    val schemaPath = Path.fromString(base) / "fileExistsSchema.csvs"
    val schemaTemplate = schemaPath.lines(includeTerminator = true).mkString
    val schema = schemaTemplate.replace("$$acceptancePath$$", base)

    "ensure the file exists on the file system" in {
      validate(TextFile(Path.fromString(base) / "fileExistsPassMetaData.csv"), parse(new StringReader(schema)), None).isSuccess mustEqual true
    }

    "ensure the file exists on the file system" in {

      val csvPath = Path.fromString(base) / "fileExistsCrossRefPassMetaData.csv"
      val csvTemplate = csvPath.lines(includeTerminator = true).mkString
      val csv = csvTemplate.replace("$$acceptancePath$$", base)

      validateR(new StringReader(csv), parse(base + "/fileExistsCrossRefSchema.csvs")).isSuccess mustEqual true
    }

    "fail if the file does not exist on the file system" in {
      validate(TextFile(Path.fromString(base) / "fileExistsPassMetaData.csv"), parse(base + "/fileExistsSchemaWithBadBasePath.csvs"), None) must beLike {
        case Failure(errors) => errors.list mustEqual IList(
          FailMessage(ValidationError, """fileExists("src/test/resources/uk/gov/nationalarchives") fails for line: 1, column: PasswordFile, value: "benPass.csvs"""",Some(1),Some(2)),
          FailMessage(ValidationError, """fileExists("src/test/resources/uk/gov/nationalarchives") fails for line: 2, column: PasswordFile, value: "andyPass.csvs"""",Some(2),Some(2)))
      }
    }

    "enforce case-sensitive comparisons when case-sensitive comparisons are set" in {

      val csfSchemaPath = Path.fromString(base) / "caseSensitiveFiles.csvs"
      val csfSchemaTemplate = csfSchemaPath.lines(includeTerminator = true).mkString
      val csfSchema = csfSchemaTemplate.replace("$$acceptancePath$$", base)

      validateE(TextFile(Path.fromString(base) / "caseSensitiveFiles.csv"), parseE(new StringReader(csfSchema)), None) must beLike {
        case Failure(errors) => errors.list mustEqual IList(
          FailMessage(ValidationError, """fileExists("$$acceptance$$") fails for line: 2, column: filename, value: "casesensitivefiles.csv"""".replace("$$acceptance$$", base),Some(2),Some(0)),
          FailMessage(ValidationError, """fileExists("$$acceptance$$") fails for line: 3, column: filename, value: "CASESENSITIVEFILES.csv"""".replace("$$acceptance$$", base),Some(3),Some(0))
        )
      }
    }
  }

  "A range rule" should {

    "enforce all element in a call on to be in a range of value" in {
      validate(TextFile(Path.fromString(base) / "rangeRulePassMetaData.csv"), parse(base + "/rangeRuleSchema.csvs"), None).isSuccess mustEqual  true
    }

    "enforce all element in a call on to be in a range of value" in {
      validate(TextFile(Path.fromString(base) / "rangeRuleFailMetaData.csv"), parse(base + "/rangeRuleSchema.csvs"), None) must beLike {
        case Failure(errors) => errors.list mustEqual IList(FailMessage(ValidationError, """range(1910,*) fails for line: 2, column: Year_of_birth, value: "1909"""",Some(2),Some(1)))
      }
    }

    "fail with no limit set" in {
      parseSchema(TextFile(Path.fromString(base) / "rangeRuleFailSchema.csvs")) must beLike {
        case Failure(errors) => errors.list mustEqual IList(FailMessage(SchemaDefinitionError, """Column: Year_of_birth: Invalid range in 'range(*,*)' at least one value needs to be defined""",None,None))
      }
    }


    "fail with inconsistent limit" in {
      parseSchema(TextFile(Path.fromString(base) / "rangeRuleInvalidSchema.csvs")) must beLike {
        case Failure(errors) => errors.list mustEqual IList(FailMessage(SchemaDefinitionError, """Column: Year_of_birth: Invalid range, minimum greater than maximum in: 'range(100,1)' at line: 4, column: 16""",None,None))
      }
    }
  }

  "A checksum rule" should {

    "enforce case-sensitive comparisons when case-sensitive comparisons are set" in {

      val csfSchemaPath = Path.fromString(base) / "caseSensitiveFilesChecksum.csvs"
      val csfSchemaTemplate = csfSchemaPath.lines(includeTerminator = true).mkString
      val csfSchema = csfSchemaTemplate.replace("$$acceptancePath$$", base)

      validateE(TextFile(Path.fromString(base) / "caseSensitiveFilesChecksum.csv"), parseE(new StringReader(csfSchema)), None) must beLike {
        case Failure(errors) => errors.list mustEqual IList(
          FailMessage(ValidationError, """checksum(file("$$acceptance$$", $filename), "MD5") file "$$acceptance$$$$file-sep$$casesensitivefileschecksum.csvs" not found for line: 2, column: checksum, value: "41424313f6052b7f062358ed38640b6e"""".replace("$$acceptance$$", base).replace("$$file-sep$$", FILE_SEPARATOR.toString),Some(2),Some(1)),
          FailMessage(ValidationError, """checksum(file("$$acceptance$$", $filename), "MD5") file "$$acceptance$$$$file-sep$$CASESENSITIVEFILESCHECKSUM.csvs" not found for line: 3, column: checksum, value: "41424313f6052b7f062358ed38640b6e"""".replace("$$acceptance$$", base).replace("$$file-sep$$", FILE_SEPARATOR.toString),Some(3),Some(1))
        )
      }
    }
  }

  "A identical rule" should {

    "enforce all rows of the same column to be identical between themselves " in {
      validate(TextFile(Path.fromString(base) / "identicalPassMetaData.csv"), parse(base + "/identicalSchema.csvs"), None).isSuccess mustEqual  true
    }

    "enforce all rows of the same column to be identical between themselves with header" in {
      validate(TextFile(Path.fromString(base) / "identicalHeaderMetaData.csv"), parse(base + "/identicalHeaderSchema.csvs"), None).isSuccess mustEqual  true
    }

    "fail for different rows in the same column  " in {
      validateE(TextFile(Path.fromString(base) / "identicalFailMetaData.csv"), parseE(base + "/identicalSchema.csvs"), None) must beLike {
        case Failure(errors) => errors.list mustEqual IList(
          FailMessage(ValidationError, """identical fails for line: 3, column: FullName, value: "fff"""",Some(3),Some(1))
        )
      }
    }

    "fail for empty value   " in {
      validate(TextFile(Path.fromString(base) / "identicalEmptyMetaData.csv"), parse(base + "/identicalSchema.csvs"), None).isFailure mustEqual true
    }

  }

  "Validate fail fast" should {
    val app = new CsvValidator with FailFastMetaDataValidator  { val pathSubstitutions = List[(String,String)](); val enforceCaseSensitivePathChecks = false; val trace = false }

    "only report first error for invalid @TotalColumns" in {
      app.validate(TextFile(Path.fromString(base) / "totalColumnsFailMetaData.csv"), parse(base + "/totalColumnsSchema.csvs"), None) must beLike {
        case Failure(errors) => errors.list mustEqual IList(FailMessage(ValidationError, "Expected @totalColumns of 1 and found 2 on line 2",Some(2),Some(2)))
      }
    }

    "only report first rule fail for multiple rules on a column" in {
      app.validate(TextFile(Path.fromString(base) / "rulesFailMetaData.csv"), parse(base + "/rulesSchema.csvs"), None) must beLike {
        case Failure(errors) => errors.list mustEqual IList(FailMessage(ValidationError, """regex("[A-Z][a-z]+") fails for line: 2, column: Name, value: "ben"""",Some(2),Some(0)))
      }
    }

    "succeed for multiple rules with valid metadata" in {
      app.validate(TextFile(Path.fromString(base) / "twoRulesPassMetaData.csv"), parse(base + "/twoRuleSchema.csvs"), None).isSuccess mustEqual true
    }


    "report warnings" in {
      app.validate(TextFile(Path.fromString(base) / "warnings.csv"), parse(base + "/warnings.csvs"), None) must beLike {
        case Failure(warnings) => warnings.list mustEqual IList(
          FailMessage(ValidationWarning, """is("WO") fails for line: 2, column: department, value: "BT"""", Some(2), Some(0)),
          FailMessage(ValidationWarning, """is("WO") fails for line: 3, column: department, value: "ED"""", Some(3), Some(0))
        )
      }
    }

    "report warnings and only first error" in {
      app.validate(TextFile(Path.fromString(base) / "warningsAndErrors.csv"), parse(base + "/warnings.csvs"), None) must beLike {
        case Failure(warningsAndError) => warningsAndError.list mustEqual IList(
          FailMessage(ValidationWarning, """is("WO") fails for line: 2, column: department, value: "BT"""", Some(2), Some(0)),
          FailMessage(ValidationWarning, """is("WO") fails for line: 3, column: department, value: "ED"""", Some(3), Some(0)),
          FailMessage(ValidationError, """is("13") fails for line: 4, column: division, value: "15"""", Some(4), Some(1))
        )
      }
    }
  }

  "validate schema" should {

    "fail with duplicate column ids" in {
      parseSchema(TextFile(Path.fromString(base) / "duplicateColumnIdsFailSchema.csvs")) must beLike {
        case Failure(errors) => errors.list mustEqual IList(FailMessage(SchemaDefinitionError, """Column: Age has duplicates on lines 3, 8
                                                                           |Column: Country has duplicates on lines 4, 5, 7""".stripMargin))
      }
    }

    "fail with unique column ids" in {
      validate(TextFile(Path.fromString(base) / "duplicateColumnIdsMetaData.csv"), parse(base + "/duplicateColumnIdsPassSchema.csvs"), None).isFailure mustEqual true
    }
  }

  "An 'or' rule" should {

    "succeed if either the lhs or rhs succeeds" in {
      validate(TextFile(Path.fromString(base) / "orWithTwoRulesPassMetaData.csv"), parse(base + "/orWithTwoRulesSchema.csvs"), None) must beLike {
        case Success(_) => ok
      }
    }

    "fail if both the lhs or rhs are fail" in {
      validate(TextFile(Path.fromString(base) / "orWithTwoRulesFailMetaData.csv"), parse(base + "/orWithTwoRulesSchema.csvs"), None) must beLike {
        case Failure(errors) => errors.list mustEqual IList(FailMessage(ValidationError, """regex("[A-Z][a-z]+") or regex("[0-9]+") fails for line: 4, column: CountryOrCountryCode, value: "Andromeda9"""",Some(4),Some(1)))
      }
    }

    "succeed for 2 'or' rules with an 'and' rule" in {
      validate(TextFile(Path.fromString(base) / "orWithFourRulesPassMetaData.csv"), parse(base + "/orWithFourRulesSchema.csvs"), None).isSuccess mustEqual true
    }

    "fail if 'or' rules pass and 'and' rule fails" in {
      validate(TextFile(Path.fromString(base) / "orWithFourRulesFailMetaData.csv"), parse(base + "/orWithFourRulesSchema.csvs"), None) must beLike {
        case Failure(errors) => errors.list mustEqual IList(FailMessage(ValidationError, """regex("[A-Z].+") fails for line: 2, column: Country, value: "ngland"""",Some(2),Some(1)))
      }
    }
  }

  "No arg standard rules" should {
    "succeed if all the rules are valid" in {
      validate(TextFile(Path.fromString(base) / "standardRulesPassMetaData.csv"), parse(base + "/standardRulesSchema.csvs"), None).isSuccess mustEqual true
    }

    "fail if all the rules are not" in {
      validate(TextFile(Path.fromString(base) / "standardRulesFailMetaData.csv"), parse(base + "/standardRulesSchema.csvs"), None) must beLike {
        case Failure(errors) => errors.list mustEqual IList(
          FailMessage(ValidationError, """uri fails for line: 1, column: uri, value: "http:##datagov.nationalarchives.gov.uk#66#WO#409#9999#0#aaaaaaaa-aaaa-4aaa-9eee-0123456789ab"""", Some(1), Some(0)),
          FailMessage(ValidationError, """xDateTime fails for line: 1, column: xDateTime, value: "2002-999-30T09:00:10"""", Some(1), Some(1)),
          FailMessage(ValidationError, """xDate fails for line: 1, column: xDate, value: "02-99-30"""", Some(1), Some(2)),
          FailMessage(ValidationError, """ukDate fails for line: 1, column: ukDate, value: "99/00/0009"""", Some(1), Some(3)),
          FailMessage(ValidationError, """xTime fails for line: 1, column: xTime, value: "99:00:889"""", Some(1), Some(4)),
          FailMessage(ValidationError, """uuid4 fails for line: 1, column: uuid4, value: "aaaaaaaab-aaaab-4aaa-9eee-0123456789ab"""", Some(1), Some(5)),
          FailMessage(ValidationError, """positiveInteger fails for line: 1, column: positiveInteger, value: "12-0912459"""", Some(1), Some(6))
        )
      }
    }
  }

  "Schema 1.1 should be backward compatible" in {
    parseSchema(TextFile(Path.fromString(base + "/rule1_0.csvs"))).isSuccess mustEqual true
  }

  "No ext string provider" should {
    "should remove filename extension" in {
      validate(TextFile(Path.fromString(base) / "noextPass.csv"), parse(base + "/noext.csvs"), None).isSuccess mustEqual true
    }

    "fail for incorrect extension removal" in {
      validate(TextFile(Path.fromString(base) / "noextFail.csv"), parse(base + "/noext.csvs"), None) must beLike {
        case Failure(errors) => errors.list mustEqual IList(FailMessage(ValidationError, """is(noext($identifier)) fails for line: 3, column: noext, value: "file:/a/b/c.txt"""",Some(3),Some(1)))
      }
    }
  }

  "Url decode string provider" should {
    "decode url string to normal string" in {
      validate(TextFile(Path.fromString(base) / "uriDecodePass.csv"), parse(base + "/uriDecode.csvs"), None).isSuccess mustEqual true
    }

    "fail for wrong url" in {
      validate(TextFile(Path.fromString(base) / "uriDecodeFail.csv"), parse(base + "/uriDecode.csvs"), None).isFailure mustEqual true
    }

    "decode URL with optional charset parameter" in {

      validate(TextFile(Path.fromString(base) / "uriDecodeWithCharsetPass.csv"), parse(base + "/uriDecodeWithCharset.csvs"), None).isSuccess mustEqual true
    }
  }

  "Concat string provider" should {
    "should concatenate string provider" in {
      validate(TextFile(Path.fromString(base) / "concatPass.csv"), parse(base + "/concat.csvs"), None).isSuccess mustEqual true
    }

    "fail for incorrect concatenation" in {
      validate(TextFile(Path.fromString(base) / "concatFail.csv"), parse(base + "/concat.csvs"), None) must beLike {
        case Failure(errors) => errors.list mustEqual IList(FailMessage(ValidationError, """is(concat($c1, $c2)) fails for line: 3, column: c3, value: "ccccc"""",Some(3),Some(2)))
      }
    }

    "should concatenate string provider (various arguments)" in {
      validate(TextFile(Path.fromString(base) / "concat4Pass.csv"), parse(base + "/concat4.csvs"), None).isSuccess mustEqual true
    }

    "fail for incorrect concatenation (various arguments)" in {
      validate(TextFile(Path.fromString(base) / "concat4Fail.csv"), parse(base + "/concat4.csvs"), None) must beLike {
        case Failure(errors) =>  errors.list mustEqual IList(FailMessage(ValidationError, """is(concat($c1, $c2, "hello", $c4)) fails for line: 2, column: c5, value: "aabbccdd"""",Some(2),Some(4)))
      }
    }
  }

  "Redacted schema" should {
    "should remove filename extension" in {
      validate(TextFile(Path.fromString(base) / "redactedPass.csv"), parse(base + "/redacted.csvs"), None).isSuccess mustEqual true
    }

    "fail for incorrect concatenation" in {
      validate(TextFile(Path.fromString(base) / "redactedFail.csv"), parse(base + "/redacted.csvs"), None) must beLike {
        case Failure(errors) => errors.list mustEqual IList(FailMessage(ValidationError, """is(concat(noext($original_identifier), "_R.pdf")) fails for line: 2, column: identifier, value: "file:/some/folder/TNA%20Digital%20Preservation%20Strategy%20v0.3%5BA1031178%5D_R1.pdf"""",Some(2),Some(0)))
      }
    }
  }

  "Reader" should {
    "successfully parse byte order marks" in {
      validate(TextFile(Path.fromString(base) / "bom.csv"), parse(base + "/bom.csvs"), None).isSuccess mustEqual true
    }
  }
}
