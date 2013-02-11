package uk.gov.tna.dri.validator

import org.specs2.mutable.Specification
import scalaz._

class MetaDataValidatorAcceptanceSpec extends Specification {

  val basePath = "src/test/resources/uk/gov/tna/dri/validator/acceptance/"

  "Regex rule" should {

    "succeed for metadata file with column that passes regex rule" in {
      MetaDataValidatorApp.validate(basePath + "regexRulePassMetaData.csv", basePath + "regexRuleSchema.txt") must beLike {
        case Success(_) => ok
      }
    }

    "fail with line number and column id in error message " in {
      MetaDataValidatorApp.validate(basePath + "regexRuleFailMetaData.csv", basePath + "regexRuleSchema.txt") must beLike {
        case Failure(errors) => errors.list must containTheSameElementsAs(List("regex: [0-9]+ fails for line 1, column: Age"))
      }
    }
  }

  "Multiple errors " should {
    "all be reported" in {
      MetaDataValidatorApp.validate(basePath + "multipleErrorsMetaData.csv", basePath + "regexRuleSchema.txt") must beLike {
        case Failure(errors) => errors.list must contain(
          "regex: [0-9]+ fails for line 1, column: Age",
          "regex: [0-9]+ fails for line 2, column: Age").only
      }
    }
  }

  "Combining two rules" should {
    "succeed" in {
      MetaDataValidatorApp.validate(basePath + "twoRulesPassMetaData.csv", basePath + "twoRuleSchema.txt") must beLike {
        case Success(_) => ok
      }
    }
  }

  "An in rule" should {
    "succeed if the column value is in the rule's literal string" in {
      MetaDataValidatorApp.validate(basePath + "inRulePassMetaData.csv", basePath + "inRuleSchema.txt") must beLike {
        case Success(_) => ok
      }
    }

    "fail if the column value is not in the rule's literal string" in {
      MetaDataValidatorApp.validate(basePath + "inRuleFailMetaData.csv", basePath + "inRuleSchema.txt") must beLike {
        case Failure(errors) => errors.list must contain(
          "inRule: thevaluemustbeinthisstring fails for line 1, column: SomeInRule, value: valuenotinrule",
          "inRule: thevaluemustbeinthisstring fails for line 3, column: SomeInRule, value: thisonewillfailtoo").only
      }
    }

    "succeed if the column value is in the rule's cross referenced column" in {
      MetaDataValidatorApp.validate(basePath + "inRuleCrossReferencePassMetaData.csv", basePath + "inRuleCrossReferenceSchema.txt") must beLike {
        case Success(_) => ok
      }
    }

    "fail if the column value is not in the rule's cross referenced column" in {
      MetaDataValidatorApp.validate(basePath + "inRuleCrossReferenceFailMetaData.csv", basePath + "inRuleCrossReferenceSchema.txt") must beLike {
        case Failure(errors) => errors.list must containTheSameElementsAs(List("inRule: David Ainslie fails for line 2, column: FirstName, value: Dave"))
      }
    }
  }

  "An @Optional column directive" should {
    "allow a column to have an empty value and ignore other rules" in {
      MetaDataValidatorApp.validate(basePath + "optionalColumnDirectivePassMetaData.csv", basePath + "optionalColumnDirectiveSchema.txt") must beLike {
        case Success(_) => ok
      }
    }

    "fail if a non empty value fails a rule" in {
      MetaDataValidatorApp.validate(basePath + "optionalColumnDirectiveFailMetaData.csv", basePath + "optionalColumnDirectiveSchema.txt") must beLike {
        case Failure(errors) => errors.list must containTheSameElementsAs(List("inRule: Benjamin Parker fails for line 1, column: Name, value: BP"))
      }
    }
  }
}
