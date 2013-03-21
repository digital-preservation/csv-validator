package uk.gov.tna.dri.validator

import org.specs2.mutable.Specification
import scalaz._
import uk.gov.tna.dri.schema.Schema


class MetaDataValidatorAcceptanceSpec extends Specification {

  val basePath = "src/test/resources/uk/gov/tna/dri/validator/acceptance/"

  val v: MetaDataValidatorApp = new MetaDataValidatorApp with AllErrorsMetaDataValidator  { val pathSubstitutions = List[(String,String)]() }
  import v.{validate, parseSchema}

  def parse(filePath: String): Schema = parseSchema(filePath) fold (f => throw new IllegalArgumentException(f.toString()), s => s)

  "Regex rule" should {

    "succeed for metadata file with column that passes regex rule" in {
      validate(basePath + "regexRulePassMetaData.csv", parse(basePath + "regexRuleSchema.txt")) must beLike {
        case Success(_) => ok
      }
    }

    "succeed when @noHeader not set as first line is skipped" in {

      validate(basePath + "regexRuleFailMetaData.csv", parse(basePath + "regexRuleSchema.txt")) must beLike {
        case Success(_) => ok
      }
    }

    "succeed when @noHeader not set" in {
      validate(basePath + "regexRuleFailMetaData.csv", parse(basePath + "regexRuleSchemaWithoutNoHeaderSet.txt")) must beLike {
        case Success(_) => ok
      }
    }
  }

  "Multiple errors " should {
    "all be reported" in {
      validate(basePath + "multipleErrorsMetaData.csv", parse(basePath + "regexRuleSchemaWithNoHeaderSet.txt")) must beLike {
        case Failure(errors) => errors.list mustEqual List(
          ErrorMessage("""regex("[0-9]+") fails for line: 1, column: Age, value: "twenty""""),
          ErrorMessage("""regex("[0-9]+") fails for line: 2, column: Age, value: "thirty""""))
      }
    }
  }

  "Combining two rules" should {
    "succeed when metadata valid" in {
      validate(basePath + "twoRulesPassMetaData.csv", parse(basePath + "twoRuleSchema.txt")) must beLike {
        case Success(_) => ok
      }
    }

    "fail when rules fail for all permutations" in {
      validate(basePath + "twoRulesFailMetaData.csv", parse(basePath + "twoRuleSchemaFail.txt")) must beLike {
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
      validate(basePath + "inRulePassMetaData.csv", parse(basePath + "inRuleSchema.txt")) must beLike {
        case Success(_) => ok
      }
    }

    "fail if the column value is not in the rule's literal string" in {
      validate(basePath + "inRuleFailMetaData.csv", parse(basePath + "inRuleSchema.txt")) must beLike {
        case Failure(errors) => errors.list mustEqual List(
          ErrorMessage("""in("thevaluemustbeinthisstring") fails for line: 1, column: SomeInRule, value: "valuenotinrule""""),
          ErrorMessage("""in("thevaluemustbeinthisstring") fails for line: 3, column: SomeInRule, value: "thisonewillfailtoo""""))
      }
    }

    "succeed if the column value is in the rule's cross referenced column" in {
      validate(basePath + "inRuleCrossReferencePassMetaData.csv", parse(basePath + "inRuleCrossReferenceSchema.txt")) must beLike {
        case Success(_) => ok
      }
    }

    "fail if the column value is not in the rule's cross referenced column" in {
      validate(basePath + "inRuleCrossReferenceFailMetaData.csv", parse(basePath + "inRuleCrossReferenceSchema.txt")) must beLike {
        case Failure(errors) => errors.list mustEqual List(ErrorMessage("""in($FullName) fails for line: 2, column: FirstName, value: "Dave""""))
      }
    }
  }

  "An @optional column directive" should {
    "allow a column to have an empty value and ignore other rules" in {
      validate(basePath + "optionalPassMetaData.csv", parse(basePath + "optionalSchema.txt")) must beLike {
        case Success(_) => ok
      }
    }

    "fail if a non empty value fails a rule" in {
      validate(basePath + "optionalFailMetaData.csv", parse(basePath + "optionalSchema.txt")) must beLike {
        case Failure(errors) => errors.list mustEqual List(ErrorMessage("in($FullName) fails for line: 1, column: Name, value: \"BP\""))
      }
    }
  }

  "An @ignoreCase column directive" should {
    "pass a rule ignoring case" in {
      validate(basePath + "ignoreCasePassMetaData.csv", parse(basePath + "ignoreCaseSchema.txt")) must beLike {
        case Success(_) => ok
      }
    }
  }

  "A fileExists rule" should {
    "ensure the file exists on the file system" in {
      validate(basePath + "fileExistsPassMetaData.csv", parse(basePath + "fileExistsSchema.txt")) must beLike {
        case Success(_) => ok
      }
    }

    "ensure the file exists on the file system" in {
      validate(basePath + "fileExistsCrossRefPassMetaData.csv", parse(basePath + "fileExistsCrossRefSchema.txt")) must beLike {
        case Success(_) => ok
      }
    }

    "fail if the file does not exist on the file system" in {
      validate(basePath + "fileExistsPassMetaData.csv", parse(basePath + "fileExistsSchemaWithBadBasePath.txt")) must beLike {
        case Failure(errors) => errors.list mustEqual List(
          ErrorMessage("""fileExists("src/test/resources/uk/gov/tna/dri") fails for line: 1, column: PasswordFile, value: "benPass.txt""""),
          ErrorMessage("""fileExists("src/test/resources/uk/gov/tna/dri") fails for line: 2, column: PasswordFile, value: "andyPass.txt""""))
      }
    }
  }

  "Validate fail fast" should {
    val app = new MetaDataValidatorApp with FailFastMetaDataValidator  { val pathSubstitutions = List[(String,String)]() }

    "only report first error for invalid @TotalColumns" in {
      app.validate(basePath + "totalColumnsFailMetaData.csv", parse(basePath + "totalColumnsSchema.txt")) must beLike {
        case Failure(errors) => errors.list mustEqual List(ErrorMessage("Expected @totalColumns of 1 and found 2 on line 2"))
      }
    }

    "only report first rule fail for multiple rules on a column" in {
      app.validate(basePath + "rulesFailMetaData.csv", parse(basePath + "rulesSchema.txt")) must beLike {
        case Failure(errors) => errors.list mustEqual List(ErrorMessage("""regex("[A-Z][a-z]+") fails for line: 2, column: Name, value: "ben""""))
      }
    }

    "succeed for multiple rules with valid metadata" in {
      app.validate(basePath + "twoRulesPassMetaData.csv", parse(basePath + "twoRuleSchema.txt")) must beLike {
        case Success(_) => ok
      }
    }
  }

  "validate schema" should {

    "fail with duplicate column ids" in {
      parseSchema(basePath + "duplicateColumnIdsFailSchema.txt" ) must beLike {
        case Failure(errors) => errors.list mustEqual List(SchemaMessage("""Column: Age has duplicates on lines 3, 8
                                                             |Column: Country has duplicates on lines 4, 5, 7""".stripMargin))
      }
    }

    "succeed with unique column ids" in {
      validate(basePath + "duplicateColumnIdsMetaData.csv", parse(basePath + "duplicateColumnIdsPassSchema.txt")) must beLike {
        case Success(_) => ok
      }
    }
  }

  "An 'or' rule" should {

    "succeed if either the lhs or rhs succeeds" in {
      validate(basePath + "orWithTwoRulesPassMetaData.csv", parse(basePath + "orWithTwoRulesSchema.txt")) must beLike {
        case Success(_) => ok
      }
    }

    "fail if both the lhs or rhs are fail" in {
      validate(basePath + "orWithTwoRulesFailMetaData.csv", parse(basePath + "orWithTwoRulesSchema.txt")) must beLike {
        case Failure(errors) => errors.list mustEqual List(ErrorMessage("""regex("[A-Z][a-z]+") or regex("[0-9]+") fails for line: 4, column: CountryOrCountryCode, value: "@@£$%^""""))
      }
    }

    "succeed for 2 'or' rules with an 'and' rule" in {
      validate(basePath + "orWithFourRulesPassMetaData.csv", parse(basePath + "orWithFourRulesSchema.txt")) must beLike {
        case Success(_) => ok
      }
    }

    "fail if 'or' rules pass and 'and' rule fails" in {
      validate(basePath + "orWithFourRulesFailMetaData.csv", parse(basePath + "orWithFourRulesSchema.txt")) must beLike {
        case Failure(errors) => errors.list mustEqual List(ErrorMessage("""regex("[A-Z].+") fails for line: 2, column: Country, value: "ngland""""))
      }
    }
  }

  "No arg standard rules" should {
    "succeed if all the rules are valid" in {
      validate(basePath + "standardRulesPassMetaData.csv", parse(basePath + "standardRulesSchema.txt")) must beLike {
        case Success(_) => ok
      }
    }

    "fail if all the rules are not" in {
      validate(basePath + "standardRulesFailMetaData.csv", parse(basePath + "standardRulesSchema.txt")) must beLike {
        case Failure(errors) => errors.list.toString() mustEqual """List(ErrorMessage(uri fails for line: 1, column: uri, value: "htt://datagov.nationalarchives.gov.uk/66/WO/409/9999/0/aaaaaaaa-aaaa-4aaa-9eee-0123456789ab"), ErrorMessage(xDateTime fails for line: 1, column: xDateTime, value: "2002-999-30T09:00:10"), ErrorMessage(xDate fails for line: 1, column: xDate, value: "02-99-30"), ErrorMessage(ukDate fails for line: 1, column: ukDate, value: "99/00/0009"), ErrorMessage(xTime fails for line: 1, column: xTime, value: "99:00:889"), ErrorMessage(uuid4 fails for line: 1, column: uuid4, value: "aaaaaaaab-aaaab-4aaa-9eee-0123456789ab"), ErrorMessage(positiveInteger fails for line: 1, column: positiveInteger, value: "12-0912459"))"""

      }
    }
  }
}