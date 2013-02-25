package uk.gov.tna.dri.validator

import org.specs2.mutable.Specification
import scalaz._
import uk.gov.tna.dri.schema.Schema


class MetaDataValidatorAcceptanceSpec extends Specification {

  val basePath = "src/test/resources/uk/gov/tna/dri/validator/acceptance/"

  val v: MetaDataValidatorApp = new MetaDataValidatorApp with AllErrorsMetaDataValidator
  import v.{validate, parseSchema}

  def getSchema(filePath: String): Schema = {
    parseSchema(filePath) match {
      case Success(s) => s

    }
  }

  "Regex rule" should {

    "succeed for metadata file with column that passes regex rule" in {
      validate(basePath + "regexRulePassMetaData.csv", getSchema(basePath + "regexRuleSchema.txt")) must beLike {
        case Success(_) => ok
      }
    }

    "succeed when @noHeader not set as first line is skipped" in {

      validate(basePath + "regexRuleFailMetaData.csv", getSchema(basePath + "regexRuleSchema.txt")) must beLike {
        case Success(_) => ok
      }
    }

    "succeed when @noHeader not set" in {
      validate(basePath + "regexRuleFailMetaData.csv", getSchema(basePath + "regexRuleSchemaWithoutNoHeaderSet.txt")) must beLike {
        case Success(_) => ok
      }
    }
  }

  "Multiple errors " should {
    "all be reported" in {
      validate(basePath + "multipleErrorsMetaData.csv", getSchema(basePath + "regexRuleSchemaWithNoHeaderSet.txt")) must beLike {
        case Failure(errors) => errors.list mustEqual List(
          """regex("[0-9]+") fails for line: 1, column: Age, value: twenty""",
          """regex("[0-9]+") fails for line: 2, column: Age, value: thirty""")
      }
    }
  }

  "Combining two rules" should {
    "succeed when metadata valid" in {
      validate(basePath + "twoRulesPassMetaData.csv", getSchema(basePath + "twoRuleSchema.txt")) must beLike {
        case Success(_) => ok
      }
    }

    "fail when rules fail for all permutations" in {
      validate(basePath + "twoRulesFailMetaData.csv", getSchema(basePath + "twoRuleSchemaFail.txt")) must beLike {
        case Failure(errors) => errors.list mustEqual List(
          """in($FullName) fails for line: 1, column: Name, value: Ben""",
          """regex("[a-z]+") fails for line: 1, column: Name, value: Ben""",
          """in($FullName) fails for line: 2, column: Name, value: Dave""",
          """regex("[a-z]+") fails for line: 2, column: Name, value: Dave""")
      }
    }
  }

  "An in rule" should {
    "succeed if the column value is in the rule's literal string" in {
      validate(basePath + "inRulePassMetaData.csv", getSchema(basePath + "inRuleSchema.txt")) must beLike {
        case Success(_) => ok
      }
    }

    "fail if the column value is not in the rule's literal string" in {
      validate(basePath + "inRuleFailMetaData.csv", getSchema(basePath + "inRuleSchema.txt")) must beLike {
        case Failure(errors) => errors.list mustEqual List(
          """in("thevaluemustbeinthisstring") fails for line: 1, column: SomeInRule, value: valuenotinrule""",
          """in("thevaluemustbeinthisstring") fails for line: 3, column: SomeInRule, value: thisonewillfailtoo""")
      }
    }

    "succeed if the column value is in the rule's cross referenced column" in {
      validate(basePath + "inRuleCrossReferencePassMetaData.csv", getSchema(basePath + "inRuleCrossReferenceSchema.txt")) must beLike {
        case Success(_) => ok
      }
    }

    "fail if the column value is not in the rule's cross referenced column" in {
      validate(basePath + "inRuleCrossReferenceFailMetaData.csv", getSchema(basePath + "inRuleCrossReferenceSchema.txt")) must beLike {
        case Failure(errors) => errors.list mustEqual List("""in($FullName) fails for line: 2, column: FirstName, value: Dave""")
      }
    }
  }

  "An @optional column directive" should {
    "allow a column to have an empty value and ignore other rules" in {
      validate(basePath + "optionalPassMetaData.csv", getSchema(basePath + "optionalSchema.txt")) must beLike {
        case Success(_) => ok
      }
    }

    "fail if a non empty value fails a rule" in {
      validate(basePath + "optionalFailMetaData.csv", getSchema(basePath + "optionalSchema.txt")) must beLike {
        case Failure(errors) => errors.list mustEqual List("in($FullName) fails for line: 1, column: Name, value: BP")
      }
    }
  }

  "An @ignoreCase column directive" should {
    "pass a rule ignoring case" in {
      validate(basePath + "ignoreCasePassMetaData.csv", getSchema(basePath + "ignoreCaseSchema.txt")) must beLike {
        case Success(_) => ok
      }
    }
  }

  "A fileExists rule" should {
    "ensure the file exists on the file system" in {
      validate(basePath + "fileExistsPassMetaData.csv", getSchema(basePath + "fileExistsSchema.txt")) must beLike {
        case Success(_) => ok
      }
    }

    "ensure the file exists on the file system" in {
      validate(basePath + "fileExistsCrossRefPassMetaData.csv", getSchema(basePath + "fileExistsCrossRefSchema.txt")) must beLike {
        case Success(_) => ok
      }
    }

    "fail if the file does not exist on the file system" in {
      validate(basePath + "fileExistsPassMetaData.csv", getSchema(basePath + "fileExistsSchemaWithBadBasePath.txt")) must beLike {
        case Failure(errors) => errors.list mustEqual List(
          """fileExists("src/test/resources/uk/gov/tna/dri") fails for line: 1, column: PasswordFile, value: benPass.txt""",
          """fileExists("src/test/resources/uk/gov/tna/dri") fails for line: 2, column: PasswordFile, value: andyPass.txt""")
      }
    }
  }

  "Validate fail fast" should {
    val app = new MetaDataValidatorApp with FailFastMetaDataValidator

    "only report first error for invalid @TotalColumns" in {
      app.validate(basePath + "totalColumnsFailMetaData.csv", getSchema(basePath + "totalColumnsSchema.txt")) must beLike {
        case Failure(errors) => errors.list mustEqual List("Expected @totalColumns of 1 and found 2 on line 2")
      }
    }

    "only report first rule fail for multiple rules on a column" in {
      app.validate(basePath + "rulesFailMetaData.csv", getSchema(basePath + "rulesSchema.txt")) must beLike {
        case Failure(errors) => errors.list mustEqual List("""regex("[A-Z][a-z]+") fails for line: 2, column: Name, value: ben""")
      }
    }

    "succeed for multiple rules with valid metadata" in {
      app.validate(basePath + "twoRulesPassMetaData.csv", getSchema(basePath + "twoRuleSchema.txt")) must beLike {
        case Success(_) => ok
      }
    }
  }

  "validate schema" should {

    "fail with duplicate column ids" in {
      parseSchema(basePath + "duplicateColumnIdsFailSchema.txt" ) must beLike {
        case Failure(errors) => errors.list mustEqual List("""Schema Parse Error:
                                                             |[8.1] failure: Column: Age has duplicates on lines 2, 7
                                                             |Column: Country has duplicates on lines 3, 4, 6
                                                             |
                                                             |
                                                             |^""".stripMargin)
      }
    }

    "succeed with unique column ids" in {
      validate(basePath + "duplicateColumnIdsMetaData.csv", getSchema(basePath + "duplicateColumnIdsPassSchema.txt")) must beLike {
        case Success(_) => ok
      }
    }
  }

  "An 'or' rule" should {

    "succeed if either the lhs or rhs succeeds" in {
      validate(basePath + "orWithTwoRulesPassMetaData.csv", getSchema(basePath + "orWithTwoRulesSchema.txt")) must beLike {
        case Success(_) => ok
      }
    }

    "fail if both the lhs or rhs are fail" in {
      validate(basePath + "orWithTwoRulesFailMetaData.csv", getSchema(basePath + "orWithTwoRulesSchema.txt")) must beLike {
        case Failure(errors) => errors.list mustEqual List("""regex("[A-Z][a-z]+") or regex("[0-9]+") fails for line: 4, column: CountryOrCountryCode, value: @@£$%^""")
      }
    }

    "succeed for 2 'or' rules with an 'and' rule" in {
      validate(basePath + "orWithFourRulesPassMetaData.csv", getSchema(basePath + "orWithFourRulesSchema.txt")) must beLike {
        case Success(_) => ok
      }
    }

    "fail if 'or' rules pass and 'and' rule fails" in {
      validate(basePath + "orWithFourRulesFailMetaData.csv", getSchema(basePath + "orWithFourRulesSchema.txt")) must beLike {
        case Failure(errors) => errors.list mustEqual List("""regex("[A-Z].+") fails for line: 2, column: Country, value: ngland""")
      }
    }
  }

  "No arg standard rules" should {
    "succeed if all the rules are valid" in {
      validate(basePath + "standardRulesPassMetaData.csv", getSchema(basePath + "standardRulesSchema.txt")) must beLike {
        case Success(_) => ok
      }
    }

    "fail if all the rules are not" in {
      validate(basePath + "standardRulesFailMetaData.csv", getSchema(basePath + "standardRulesSchema.txt")) must beLike {
        case Failure(errors) => errors.list.toString() mustEqual """List(uri fails for line: 1, column: uri, value: htt://datagov.nationalarchives.gov.uk/66/WO/409/9999/0/aaaaaaaa-aaaa-4aaa-9eee-0123456789ab, xDateTime fails for line: 1, column: xDateTime, value: 2002-999-30T09:00:10, xDate fails for line: 1, column: xDate, value: 02-99-30, xTime fails for line: 1, column: xTime, value: 99:00:889, uuid4 fails for line: 1, column: uuid4, value: aaaaaaaab-aaaab-4aaa-9eee-0123456789ab, positiveInteger fails for line: 1, column: positiveInteger, value: 12-0912459)"""
      }
    }
  }
}