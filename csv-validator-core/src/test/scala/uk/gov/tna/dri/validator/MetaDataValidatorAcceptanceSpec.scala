package uk.gov.tna.dri.validator

import org.specs2.mutable.Specification
import scalaz._

class MetaDataValidatorAcceptanceSpec extends Specification {

  val basePath = "src/test/resources/uk/gov/tna/dri/validator/acceptance/"

  val v: MetaDataValidatorApp = new MetaDataValidatorApp with AllErrorsMetaDataValidator
  import v.validate
  
  "Regex rule" should {

    "succeed for metadata file with column that passes regex rule" in {
      validate(basePath + "regexRulePassMetaData.csv", basePath + "regexRuleSchema.txt") must beLike {
        case Success(_) => ok
      }
    }

    "fail with line number and column id in error message " in {
      validate(basePath + "regexRuleFailMetaData.csv", basePath + "regexRuleSchema.txt") must beLike {
        case Failure(errors) => errors.list mustEqual List("regex: [0-9]+ fails for line: 1, column: Age, value: twenty")
      }
    }
  }

  "Multiple errors " should {
    "all be reported" in {
      validate(basePath + "multipleErrorsMetaData.csv", basePath + "regexRuleSchema.txt") must beLike {
        case Failure(errors) => errors.list mustEqual List(
          "regex: [0-9]+ fails for line: 1, column: Age, value: twenty",
          "regex: [0-9]+ fails for line: 2, column: Age, value: thirty")
      }
    }
  }

  "Combining two rules" should {
    "succeed when metadata valid" in {
      validate(basePath + "twoRulesPassMetaData.csv", basePath + "twoRuleSchema.txt") must beLike {
        case Success(_) => ok
      }
    }

    "fail when rules fail for all permutations" in {
      validate(basePath + "twoRulesFailMetaData.csv", basePath + "twoRuleSchemaFail.txt") must beLike {
        case Failure(errors) => errors.list mustEqual List(
          "in: ben parker fails for line 1, column: Name, value: Ben",
          "regex: [a-z]+ fails for line: 1, column: Name, value: Ben",
          "in: david ainslie fails for line 2, column: Name, value: Dave",
          "regex: [a-z]+ fails for line: 2, column: Name, value: Dave")
      }
    }
  }

  "An in rule" should {
    "succeed if the column value is in the rule's literal string" in {
      validate(basePath + "inRulePassMetaData.csv", basePath + "inRuleSchema.txt") must beLike {
        case Success(_) => ok
      }
    }

    "fail if the column value is not in the rule's literal string" in {
      validate(basePath + "inRuleFailMetaData.csv", basePath + "inRuleSchema.txt") must beLike {
        case Failure(errors) => errors.list mustEqual List(
          "in: thevaluemustbeinthisstring fails for line 1, column: SomeInRule, value: valuenotinrule",
          "in: thevaluemustbeinthisstring fails for line 3, column: SomeInRule, value: thisonewillfailtoo")
      }
    }

    "succeed if the column value is in the rule's cross referenced column" in {
      validate(basePath + "inRuleCrossReferencePassMetaData.csv", basePath + "inRuleCrossReferenceSchema.txt") must beLike {
        case Success(_) => ok
      }
    }

    "fail if the column value is not in the rule's cross referenced column" in {
      validate(basePath + "inRuleCrossReferenceFailMetaData.csv", basePath + "inRuleCrossReferenceSchema.txt") must beLike {
        case Failure(errors) => errors.list mustEqual List("in: David Ainslie fails for line 2, column: FirstName, value: Dave")
      }
    }
  }

  "An @Optional column directive" should {
    "allow a column to have an empty value and ignore other rules" in {
      validate(basePath + "optionalPassMetaData.csv", basePath + "optionalSchema.txt") must beLike {
        case Success(_) => ok
      }
    }

    "fail if a non empty value fails a rule" in {
      validate(basePath + "optionalFailMetaData.csv", basePath + "optionalSchema.txt") must beLike {
        case Failure(errors) => errors.list mustEqual List("in: Benjamin Parker fails for line 1, column: Name, value: BP")
      }
    }
  }

  "An @IgnoreCase column directive" should {
    "pass a rule ignoring case" in {
      validate(basePath + "ignoreCasePassMetaData.csv", basePath + "ignoreCaseSchema.txt") must beLike {
        case Success(_) => ok
      }
    }
  }

  "A fileExists rule" should {
    "ensure the file exists on the file system" in {
      validate(basePath + "fileExistsPassMetaData.csv", basePath + "fileExistsSchema.txt") must beLike {
        case Success(_) => ok
      }
    }

    "fail if the file does not exist on the file system" in {
      validate(basePath + "fileExistsPassMetaData.csv", basePath + "fileExistsSchemaWithBadBasePath.txt") must beLike {
        case Failure(errors) => errors.list mustEqual List(
          "fileExists: fails for line: 1, column: PasswordFile, value: benPass.txt",
          "fileExists: fails for line: 2, column: PasswordFile, value: andyPass.txt")
      }
    }
  }

  "Validate fail fast" should {
    val app = new MetaDataValidatorApp with FailFastMetaDataValidator

    "only report first error for invalid @TotalColumns" in {
      app.validate(basePath + "totalColumnsFailMetaData.csv", basePath + "totalColumnsSchema.txt") must beLike {
        case Failure(errors) => errors.list mustEqual List("Expected @TotalColumns of 1 and found 2 on line 2")
      }
    }

    "only report first rule fail for multiple rules on a column" in {
      app.validate(basePath + "rulesFailMetaData.csv", basePath + "rulesSchema.txt") must beLike {
        case Failure(errors) => errors.list mustEqual List("regex: [A-Z][a-z]+ fails for line: 2, column: Name, value: ben")
      }
    }

    "succeed for multiple rules with valid metadata" in {
      app.validate(basePath + "twoRulesPassMetaData.csv", basePath + "twoRuleSchema.txt") must beLike {
        case Success(_) => ok
      }
    }
  }
}