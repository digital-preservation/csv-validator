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
import uk.gov.nationalarchives.csv.validator.api.{TextFile, CsvValidator}
import scalax.file.Path
import java.io.StringReader
import java.io


class MetaDataValidatorAcceptanceSpec extends Specification with TestResources {

  val base = acceptancePath

  val v = new CsvValidator with AllErrorsMetaDataValidator {
    val pathSubstitutions = List[(String,String)]()
    val enforceCaseSensitivePathChecks = false

    def validateR(csv: io.Reader, schema: Schema): this.type#MetaDataValidation[Any] = validate(csv, schema, None)
  }

  val ve = new CsvValidator with AllErrorsMetaDataValidator {
    val pathSubstitutions = List[(String,String)]()
    val enforceCaseSensitivePathChecks = true
  }

  import v.{validate, validateR, parseSchema}
  import ve.{validate => validateE, parseSchema => parseSchemaE}

  def parse(filePath: String): Schema = parseSchema(TextFile(Path.fromString(filePath))) fold (f => throw new IllegalArgumentException(f.toString()), s => s)
  def parseE(filePath: String): Schema = parseSchemaE(TextFile(Path.fromString(filePath))) fold (f => throw new IllegalArgumentException(f.toString()), s => s)

  def parse(reader: io.Reader): Schema = parseSchema(reader) fold (f => throw new IllegalArgumentException(f.toString()), s => s)
  def parseE(reader: io.Reader): Schema = parseSchemaE(reader) fold (f => throw new IllegalArgumentException(f.toString()), s => s)

  "@separator global directive" should {
    "succeed for '$' separator" in {
      validate(TextFile(Path.fromString(base) / "separated1.dsv"), parse(base + "/separated1.csvs"), None) must beLike {
        case Success(_) => ok
      }
    }

    "succeed for TAB separator" in {
      validate(TextFile(Path.fromString(base) / "separated2.tsv"), parse(base + "/separated2.csvs"), None) must beLike {
        case Success(_) => ok
      }
    }

    "succeed for '\t' separator" in {
      validate(TextFile(Path.fromString(base) / "separated2.tsv"), parse(base + "/separated2-1.csvs"), None) must beLike {
        case Success(_) => ok
      }
    }

    "with @quoted global directive" should {
      "succeed for '$' separator" in {
        validate(TextFile(Path.fromString(base) / "separated3.dsv"), parse(base + "/separated3.csvs"), None) must beLike {
          case Success(_) => ok
        }
      }

      "succeed for TAB separator" in {
        validate(TextFile(Path.fromString(base) / "separated4.tsv"), parse(base + "/separated4.csvs"), None) must beLike {
          case Success(_) => ok
        }
      }

      "succeed for '\t' separator" in {
        validate(TextFile(Path.fromString(base) / "separated4.tsv"), parse(base + "/separated4-1.csvs"), None) must beLike {
          case Success(_) => ok
        }
      }
    }
  }

  "Regex rule" should {

    "succeed for metadata file with column that passes regex rule" in {
      validate(TextFile(Path.fromString(base) / "regexRulePassMetaData.csv"), parse(base + "/regexRuleSchema.csvs"), None) must beLike {
        case Success(_) => ok
      }
    }

    "succeed when @noHeader not set as first line is skipped" in {

      validate(TextFile(Path.fromString(base) / "regexRuleFailMetaData.csv"), parse(base + "/regexRuleSchema.csvs"), None) must beLike {
        case Success(_) => ok
      }
    }

    "succeed when @noHeader not set" in {
      validate(TextFile(Path.fromString(base) / "regexRuleFailMetaData.csv"), parse(base + "/regexRuleSchemaWithoutNoHeaderSet.csvs"), None) must beLike {
        case Success(_) => ok
      }
    }
  }

  "Multiple errors " should {
    "all be reported" in {
      validate(TextFile(Path.fromString(base) / "multipleErrorsMetaData.csv"), parse(base + "/regexRuleSchemaWithNoHeaderSet.csvs"), None) must beLike {
        case Failure(errors) => errors.list mustEqual List(
          ErrorMessage("""regex("[0-9]+") fails for line: 1, column: Age, value: "twenty"""",1,1),
          ErrorMessage("""regex("[0-9]+") fails for line: 2, column: Age, value: "thirty"""",2,1))
      }
    }
  }

  "Combining two rules" should {
    "succeed when metadata valid" in {
      validate(TextFile(Path.fromString(base) / "twoRulesPassMetaData.csv"), parse(base + "/twoRuleSchema.csvs"), None) must beLike {
        case Success(_) => ok
      }
    }

    "fail when rules fail for all permutations" in {
      validate(TextFile(Path.fromString(base) / "twoRulesFailMetaData.csv"), parse(base + "/twoRuleSchemaFail.csvs"), None) must beLike {
        case Failure(errors) => errors.list mustEqual List(
          ErrorMessage("""in($FullName) fails for line: 1, column: Name, value: "Ben"""",1,0),
          ErrorMessage("""regex("[a-z]+") fails for line: 1, column: Name, value: "Ben"""",1,0),
          ErrorMessage("""in($FullName) fails for line: 2, column: Name, value: "Dave"""",2,0),
          ErrorMessage("""regex("[a-z]+") fails for line: 2, column: Name, value: "Dave"""",2,0))
      }
    }
  }

  "An in rule" should {
    "succeed if the column value is in the rule's literal string" in {
      validate(TextFile(Path.fromString(base) / "inRulePassMetaData.csv"), parse(base + "/inRuleSchema.csvs"), None) must beLike {
        case Success(_) => ok
      }
    }

    "fail if the column value is not in the rule's literal string" in {
      validate(TextFile(Path.fromString(base) / "inRuleFailMetaData.csv"), parse(base + "/inRuleSchema.csvs"), None) must beLike {
        case Failure(errors) => errors.list mustEqual List(
          ErrorMessage("""in("thevaluemustbeinthisstring") fails for line: 1, column: SomeInRule, value: "valuenotinrule"""",1,1),
          ErrorMessage("""in("thevaluemustbeinthisstring") fails for line: 3, column: SomeInRule, value: "thisonewillfailtoo"""",3,1))
      }
    }

    "succeed if the column value is in the rule's cross referenced column" in {
      validate(TextFile(Path.fromString(base) / "inRuleCrossReferencePassMetaData.csv"), parse(base + "/inRuleCrossReferenceSchema.csvs"), None) must beLike {
        case Success(_) => ok
      }
    }

    "fail if the column value is not in the rule's cross referenced column" in {
      validate(TextFile(Path.fromString(base) / "inRuleCrossReferenceFailMetaData.csv"), parse(base + "/inRuleCrossReferenceSchema.csvs"), None) must beLike {
        case Failure(errors) => errors.list mustEqual List(ErrorMessage("""in($FullName) fails for line: 2, column: FirstName, value: "Dave"""",2,0))
      }
    }
  }

  "An @optional column directive" should {
    "allow a column to have an empty value and ignore other rules" in {
      validate(TextFile(Path.fromString(base) / "optionalPassMetaData.csv"), parse(base + "/optionalSchema.csvs"), None) must beLike {
        case Success(_) => ok
      }
    }

    "fail if a non empty value fails a rule" in {
      validate(TextFile(Path.fromString(base) / "optionalFailMetaData.csv"), parse(base + "/optionalSchema.csvs"), None) must beLike {
        case Failure(errors) => errors.list mustEqual List(ErrorMessage("in($FullName) fails for line: 1, column: Name, value: \"BP\"",1,0))
      }
    }
  }

  "An @ignoreCase column directive" should {
    "pass a rule ignoring case" in {
      validate(TextFile(Path.fromString(base) / "ignoreCasePassMetaData.csv"), parse(base + "/ignoreCaseSchema.csvs"), None) must beLike {
        case Success(_) => ok
      }
    }
  }

  "A fileExists rule" should {

    val schemaPath = Path.fromString(base) / "fileExistsSchema.csvs"
    val schemaTemplate = schemaPath.lines(includeTerminator = true).mkString
    val schema = schemaTemplate.replace("$$acceptancePath$$", base)

    "ensure the file exists on the file system" in {
      validate(TextFile(Path.fromString(base) / "fileExistsPassMetaData.csv"), parse(new StringReader(schema)), None) must beLike {
        case Success(_) => ok
      }
    }

    "ensure the file exists on the file system" in {

      val csvPath = Path.fromString(base) / "fileExistsCrossRefPassMetaData.csv"
      val csvTemplate = csvPath.lines(includeTerminator = true).mkString
      val csv = csvTemplate.replace("$$acceptancePath$$", base)

      validateR(new StringReader(csv), parse(base + "/fileExistsCrossRefSchema.csvs")) must beLike {
        case Success(_) => ok
      }
    }

    "fail if the file does not exist on the file system" in {
      validate(TextFile(Path.fromString(base) / "fileExistsPassMetaData.csv"), parse(base + "/fileExistsSchemaWithBadBasePath.csvs"), None) must beLike {
        case Failure(errors) => errors.list mustEqual List(
          ErrorMessage("""fileExists("src/test/resources/uk/gov/nationalarchives") fails for line: 1, column: PasswordFile, value: "benPass.csvs"""",1,2),
          ErrorMessage("""fileExists("src/test/resources/uk/gov/nationalarchives") fails for line: 2, column: PasswordFile, value: "andyPass.csvs"""",2,2))
      }
    }

    "enforce case-sensitive comparisons when case-sensitive comparisons are set" in {

      val csfSchemaPath = Path.fromString(base) / "caseSensitiveFiles.csvs"
      val csfSchemaTemplate = csfSchemaPath.lines(includeTerminator = true).mkString
      val csfSchema = csfSchemaTemplate.replace("$$acceptancePath$$", base)

      validateE(TextFile(Path.fromString(base) / "caseSensitiveFiles.csv"), parseE(new StringReader(csfSchema)), None) must beLike {
        case Failure(errors) => errors.list mustEqual List(
          ErrorMessage("""fileExists("$$acceptance$$") fails for line: 2, column: filename, value: "casesensitivefiles.csv"""".replace("$$acceptance$$", base),2,0),
          ErrorMessage("""fileExists("$$acceptance$$") fails for line: 3, column: filename, value: "CASESENSITIVEFILES.csv"""".replace("$$acceptance$$", base),3,0)
        )
      }
    }
  }

  "A checksum rule" should {

    "enforce case-sensitive comparisons when case-sensitive comparisons are set" in {

      val csfSchemaPath = Path.fromString(base) / "caseSensitiveFilesChecksum.csvs"
      val csfSchemaTemplate = csfSchemaPath.lines(includeTerminator = true).mkString
      val csfSchema = csfSchemaTemplate.replace("$$acceptancePath$$", base)

      validateE(TextFile(Path.fromString(base) / "caseSensitiveFilesChecksum.csv"), parseE(new StringReader(csfSchema)), None) must beLike {
        case Failure(errors) => errors.list mustEqual List(
          ErrorMessage("""checksum(file("$$acceptance$$", $filename), "MD5") file "$$acceptance$$$$file-sep$$casesensitivefileschecksum.csvs" not found for line: 2, column: checksum, value: "41424313f6052b7f062358ed38640b6e"""".replace("$$acceptance$$", base).replace("$$file-sep$$", FILE_SEPARATOR.toString),2,1),
          ErrorMessage("""checksum(file("$$acceptance$$", $filename), "MD5") file "$$acceptance$$$$file-sep$$CASESENSITIVEFILESCHECKSUM.csvs" not found for line: 3, column: checksum, value: "41424313f6052b7f062358ed38640b6e"""".replace("$$acceptance$$", base).replace("$$file-sep$$", FILE_SEPARATOR.toString),3,1)
        )
      }
    }
  }

  "Validate fail fast" should {
    val app = new CsvValidator with FailFastMetaDataValidator  { val pathSubstitutions = List[(String,String)](); val enforceCaseSensitivePathChecks = false }

    "only report first error for invalid @TotalColumns" in {
      app.validate(TextFile(Path.fromString(base) / "totalColumnsFailMetaData.csv"), parse(base + "/totalColumnsSchema.csvs"), None) must beLike {
        case Failure(errors) => errors.list mustEqual List(ErrorMessage("Expected @totalColumns of 1 and found 2 on line 2",2,2))
      }
    }

    "only report first rule fail for multiple rules on a column" in {
      app.validate(TextFile(Path.fromString(base) / "rulesFailMetaData.csv"), parse(base + "/rulesSchema.csvs"), None) must beLike {
        case Failure(errors) => errors.list mustEqual List(ErrorMessage("""regex("[A-Z][a-z]+") fails for line: 2, column: Name, value: "ben"""",2,0))
      }
    }

    "succeed for multiple rules with valid metadata" in {
      app.validate(TextFile(Path.fromString(base) / "twoRulesPassMetaData.csv"), parse(base + "/twoRuleSchema.csvs"), None) must beLike {
        case Success(_) => ok
      }
    }
  }

  "validate schema" should {

    "fail with duplicate column ids" in {
//      parseSchema(Path.fromString(base) / "duplicateColumnIdsFailSchema.csvs" ) must beLike {
//        case Failure(errors) => errors.list mustEqual List(SchemaMessage("""Column: Age has duplicates on lines 3, 8
//                                                             |Column: Country has duplicates on lines 4, 5, 7""".stripMargin), None)
//      }

      //TODO not yet sure why we have to remove the training `None`
      parseSchema(TextFile(Path.fromString(base) / "duplicateColumnIdsFailSchema.csvs")) must beLike {
        case Failure(errors) => errors.list mustEqual List(SchemaMessage("""Column: Age has duplicates on lines 3, 8
                                                                           |Column: Country has duplicates on lines 4, 5, 7""".stripMargin))
      }
    }

    "succeed with unique column ids" in {
      validate(TextFile(Path.fromString(base) / "duplicateColumnIdsMetaData.csv"), parse(base + "/duplicateColumnIdsPassSchema.csvs"), None) must beLike {
        case Success(_) => ok
      }
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
        case Failure(errors) => errors.list mustEqual List(ErrorMessage("""regex("[A-Z][a-z]+") or regex("[0-9]+") fails for line: 4, column: CountryOrCountryCode, value: "Andromeda9"""",4,1))
      }
    }

    "succeed for 2 'or' rules with an 'and' rule" in {
      validate(TextFile(Path.fromString(base) / "orWithFourRulesPassMetaData.csv"), parse(base + "/orWithFourRulesSchema.csvs"), None) must beLike {
        case Success(_) => ok
      }
    }

    "fail if 'or' rules pass and 'and' rule fails" in {
      validate(TextFile(Path.fromString(base) / "orWithFourRulesFailMetaData.csv"), parse(base + "/orWithFourRulesSchema.csvs"), None) must beLike {
        case Failure(errors) => errors.list mustEqual List(ErrorMessage("""regex("[A-Z].+") fails for line: 2, column: Country, value: "ngland"""",2,1))
      }
    }
  }

  "No arg standard rules" should {
    "succeed if all the rules are valid" in {
      validate(TextFile(Path.fromString(base) / "standardRulesPassMetaData.csv"), parse(base + "/standardRulesSchema.csvs"), None) must beLike {
        case Success(_) => ok
      }
    }

    "fail if all the rules are not" in {
      validate(TextFile(Path.fromString(base) / "standardRulesFailMetaData.csv"), parse(base + "/standardRulesSchema.csvs"), None) must beLike {
        case Failure(errors) => errors.list.toString() mustEqual """List(ErrorMessage(uri fails for line: 1, column: uri, value: "http:##datagov.nationalarchives.gov.uk#66#WO#409#9999#0#aaaaaaaa-aaaa-4aaa-9eee-0123456789ab",1,0), ErrorMessage(xDateTime fails for line: 1, column: xDateTime, value: "2002-999-30T09:00:10",1,1), ErrorMessage(xDate fails for line: 1, column: xDate, value: "02-99-30",1,2), ErrorMessage(ukDate fails for line: 1, column: ukDate, value: "99/00/0009",1,3), ErrorMessage(xTime fails for line: 1, column: xTime, value: "99:00:889",1,4), ErrorMessage(uuid4 fails for line: 1, column: uuid4, value: "aaaaaaaab-aaaab-4aaa-9eee-0123456789ab",1,5), ErrorMessage(positiveInteger fails for line: 1, column: positiveInteger, value: "12-0912459",1,6))"""

      }
    }
  }
}