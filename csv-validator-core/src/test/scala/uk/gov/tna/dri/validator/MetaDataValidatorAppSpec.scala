package uk.gov.tna.dri.validator

import org.specs2.mutable.{Before, BeforeAfter, Specification}
import scalaz._
import uk.gov.tna.dri.schema.Schema
import java.io.StringReader
import java.security.Permission

class MetaDataValidatorAppSpec extends Specification {

  val basePath = "src/test/resources/uk/gov/tna/dri/validator/"

  "Check arguments" should {
    "give usage message when no arguments supplied" in {
      MetaDataValidatorCommandLineApp.checkFileArguments(Nil) must beLike {
        case Failure(errors) => errors.head mustEqual "Usage: validate [--fail-fast] <meta-data file path> <schema file path>"
      }
    }

    "give usage message when one argument supplied" in {
      MetaDataValidatorCommandLineApp.checkFileArguments(List("meta file")) must beLike {
        case Failure(errors) => errors.head mustEqual "Usage: validate [--fail-fast] <meta-data file path> <schema file path>"
      }
    }

    "give usage message when too many arguments supplied" in {
      MetaDataValidatorCommandLineApp.checkFileArguments(List("somMetaData.csv", "someSchema.txt", "something extra")) must beLike {
        case Failure(errors) => errors.head mustEqual "Usage: validate [--fail-fast] <meta-data file path> <schema file path>"
      }
    }

    "fail if metadata file is unreadable" in {
      MetaDataValidatorCommandLineApp.checkFileArguments(List("nonExistentMetaData.csv", basePath + "schema.txt")) must beLike {
        case Failure(errors) => errors.head mustEqual "Unable to read file : nonExistentMetaData.csv"
      }
    }

    "fail if schema file is unreadable" in {
      MetaDataValidatorCommandLineApp.checkFileArguments(List(basePath + "metaData.csv", "nonExistentSchema.txt")) must beLike {
        case Failure(errors) => errors.head mustEqual "Unable to read file : nonExistentSchema.txt"
      }
    }

    "fail if both metadata and schema file are unreadable" in {
      MetaDataValidatorCommandLineApp.checkFileArguments(List("nonExistentmetaData.csv", "nonExistentSchema.txt")) must beLike {
        case Failure(errors) => errors.list must contain("Unable to read file : nonExistentmetaData.csv", "Unable to read file : nonExistentSchema.txt")
      }
    }

    "succeed if both metadata and schema file are readable" in {
      MetaDataValidatorCommandLineApp.checkFileArguments(List(basePath + "metaData.csv", basePath + "schema.txt")) must beLike {
        case Success(_) => ok
      }
    }
  }

  "Fail fast and file args" should {

    "return true and the file names for fail fast" in {
      MetaDataValidatorCommandLineApp.failFastAndFileArgs(List("--fail-fast", "someMetaData.csv", "someSchema.txt")) mustEqual (true, List("someMetaData.csv", "someSchema.txt"))
    }

    "return true and the file names for fail fast short form" in {
      MetaDataValidatorCommandLineApp.failFastAndFileArgs(List("-f", "someMetaData.csv", "someSchema.txt")) mustEqual (true, List("someMetaData.csv", "someSchema.txt"))
    }

    "return false and the file names for no fail fast" in {
      MetaDataValidatorCommandLineApp.failFastAndFileArgs(List("someMetaData.csv", "someSchema.txt")) mustEqual (false, List("someMetaData.csv", "someSchema.txt"))
    }
  }

  "Parsing schema" should {
    val app = new MetaDataValidatorApp with AllErrorsMetaDataValidator

    "report position on parse fail" in {

      val schema =
        """version 1.0
          |@totalColumns 1
          |Name: regox("A")
        """.stripMargin

      app.parseAndValidate(new StringReader(schema)) must beLike {
        case Failure(msgs) => msgs.list mustEqual List(
          """[3.7] failure: Column definition contains invalid text
            |
            |Name: regox("A")
            |
            |      ^""".stripMargin)
      }
    }
  }

  "Validation" should {
    val app = new MetaDataValidatorApp with AllErrorsMetaDataValidator

    def parse(filePath: String): Schema = app.parseSchema(filePath) fold (f => throw new IllegalArgumentException(f.toString), s => s)

    "succeed for valid schema and metadata file" in {
      app.validate(basePath + "metaData.csv", parse(basePath + "schema.txt")) must beLike {
        case Success(_) => ok
      }
    }

    "succeed for valid @totalColumns in schema and metadata file" in {
      app.validate(basePath + "metaData.csv", parse(basePath + "schema.txt")) must beLike {
        case Success(_) => ok
      }
    }
  }
}

class ExitStatus extends Specification {
  "exit status should equal 1 when the command line arguments are wrong" in new WithSecurity with Before{
    MetaDataValidatorCommandLineApp.main(Array("")) must throwA[RuntimeException] (message = "1")
  }

  "exit status should equal 2 when the schema is invalid" in new WithSecurity with Before {
    MetaDataValidatorCommandLineApp.main(Array("src/test/resources/uk/gov/tna/dri/validator/metaData.csv",
      "src/test/resources/uk/gov/tna/dri/validator/badSchema.txt")) must throwA[RuntimeException] (message = "2")
  }

  "exit status should equal 3 when the csv is invalid" in new WithSecurity with BeforeAfter {
    MetaDataValidatorCommandLineApp.main(Array("src/test/resources/uk/gov/tna/dri/validator/acceptance/standardRulesFailMetaData.csv",
      "src/test/resources/uk/gov/tna/dri/validator/acceptance/standardRulesSchema.txt")) must throwA[RuntimeException] (message = "3")
  }
}

trait WithSecurity {
  def after: Any = {
    System.setSecurityManager( null )
  }
  def before: Any = {
    println(System.getSecurityManager)
    System.setSecurityManager(new TestSecurityManager)
  }
}

private class TestSecurityManager extends SecurityManager {

  override def checkExit(status: Int) {
    throw new RuntimeException(status.toString)
  }


  override def checkPermission(perm: Permission ) {
    //allow everything
  }

  override def checkPermission(perm: Permission , context: AnyRef ) {
    //allow everything
  }
}
