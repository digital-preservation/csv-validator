package uk.gov.tna.dri.validator

import org.specs2.mutable.Specification
import scalaz._
import uk.gov.tna.dri.schema.Schema
import java.io.StringReader

class MetaDataValidatorAppSpec extends Specification {

  val basePath = "src/test/resources/uk/gov/tna/dri/validator/"

  "Check arguments" should {
    "give usage message when no arguments supplied" in {
      MetaDataValidatorCommandLineApp.run(Array.empty) must beLike {
        case (errMsg,errCode) => errMsg mustEqual """Usage: validate [--fail-fast] [--path <from> <to>]* <meta-data file path> <schema file path>"""
      }
    }

    "give usage message when one argument supplied" in {
      MetaDataValidatorCommandLineApp.run(Array("meta file")) must beLike {
        case (errMsg,errCode) => errMsg mustEqual """Usage: validate [--fail-fast] [--path <from> <to>]* <meta-data file path> <schema file path>"""
      }
    }

//    "give usage message when too many arguments supplied" in {
//      MetaDataValidatorCommandLineApp.run(List("somMetaData.csv", "someSchema.txt", "something extra").toArray) must beLike {
//        case (errMsg,errCode) => errMsg mustEqual """Usage: validate [--fail-fast] [--path <from> <to>]* <meta-data file path> <schema file path>"""
//      }
//    }

    "fail if metadata file is unreadable" in {
      MetaDataValidatorCommandLineApp.run(Array("nonExistentMetaData.csv", basePath + "schema.txt")) must beLike {
        case (errMsg,errCode) => errMsg mustEqual "Unable to read file : nonExistentMetaData.csv"
      }
    }

    "fail if schema file is unreadable" in {
      MetaDataValidatorCommandLineApp.run(Array(basePath + "metaData.csv", "nonExistentSchema.txt")) must beLike {
        case (errMsg,errCode) => errMsg mustEqual "Unable to read file : nonExistentSchema.txt"
      }
    }

    "fail if both metadata and schema file are unreadable" in {
      MetaDataValidatorCommandLineApp.run(Array("nonExistentmetaData.csv", "nonExistentSchema.txt")) must beLike {
        case (errMsg,errCode) => errMsg mustEqual "Unable to read file : nonExistentmetaData.csv\nUnable to read file : nonExistentSchema.txt"
      }
    }

    "succeed if both metadata and schema file are readable" in {
      MetaDataValidatorCommandLineApp.run(Array(basePath + "metaData.csv", basePath + "schema.txt")) must beLike {
        case (errMsg,errCode) => errMsg mustEqual "PASS"
      }
    }
  }

  "Fail fast and file args" should {

    "return true and the file names for fail fast" in {
      MetaDataValidatorCommandLineApp.run(Array("--fail-fast", "someMetaData.csv", "someSchema.txt")) mustEqual ("Unable to read file : someMetaData.csv\nUnable to read file : someSchema.txt",1)
    }

    "return true and the file names for fail fast short form" in {
      MetaDataValidatorCommandLineApp.run(Array("-f", "someMetaData.csv", "someSchema.txt")) mustEqual ("Unable to read file : someMetaData.csv\nUnable to read file : someSchema.txt",1)
    }

    "return false and the file names for no fail fast" in {
      MetaDataValidatorCommandLineApp.run(Array("someMetaData.csv", "someSchema.txt")) mustEqual ("Unable to read file : someMetaData.csv\nUnable to read file : someSchema.txt",1)
    }
  }

  "--path and file args" should {
//    "find " {
//
//    }

//    "handle --path option" in {
//      val commandLine = List[String](
//        "--path", "c:", "",
//        "--path", """file://c:""", """file://""",
//        "--fail-fast"
//      )
//      MetaDataValidatorCommandLineApp.findSubstitutionPaths(commandLine) mustEqual Success(List( ("c:", ""), ("file://c:", "file://")) )
//    }

  }

  "Given a list of args" should {
   "be able to find all the paths" in {
     MetaDataValidatorCommandLineApp.findPaths( List("--path", "hello", "world")) mustEqual Right( (List( ("hello", "world") ), List()))
   }

    "be able to find the path and return remainders" in {
      MetaDataValidatorCommandLineApp.findPaths( List("123", "--path", "hello", "world", "xyz")) mustEqual Right( (List( ("hello", "world") ), List("123", "xyz")))
    }

    "be able to find multiple paths and return remainders" in {
      MetaDataValidatorCommandLineApp.findPaths( List("123", "--path", "hello", "world", "xyz", "--path", "hello2", "world2")) mustEqual Right( (List( ("hello", "world"), ("hello2", "world2") ), List("123", "xyz")))
    }


    "be able to handle a missing value" in {
      MetaDataValidatorCommandLineApp.findPaths( List("--path", "hello" )) mustEqual Left("Missing param to --path\nUsage: validate [--fail-fast] [--path <from> <to>]* <meta-data file path> <schema file path>")
    }

    "find the --fail-fast option" in {
      MetaDataValidatorCommandLineApp.findFailFast( List("--fail-fast", "hello", "world")) mustEqual Right( true, List("hello", "world"))
    }

    "find find the metafile and schema" in {
      MetaDataValidatorCommandLineApp.findFiles( List("--fail-fast", basePath + "metaData.csv", basePath + "schema.txt")) mustEqual Right( (basePath + "metaData.csv", basePath + "schema.txt"), List("--fail-fast"))
    }


  }


  "Command line app" should {

    "have exit code 0 when validation successful" in {
      MetaDataValidatorCommandLineApp.run(Array(basePath + "metaData.csv", basePath + "schema.txt")) mustEqual Tuple2("PASS", 0)
    }

//    "have exit code 0 when validation --path successful" in {
//      MetaDataValidatorCommandLineApp.run(Array( "--path", "c:", "", basePath + "metaData.csv", basePath + "schema.txt"))._2 mustEqual 0
//    }

    "have exit code 1 when the command line arguments are wrong" in {
      MetaDataValidatorCommandLineApp.run(Array(""))._2 mustEqual 1
    }

    "have exit code 1 when the --path is missing one args " in {
      MetaDataValidatorCommandLineApp.run(Array("--path", "c:", basePath + "metaData.csv", basePath + "schema.txt"))._2 mustEqual 1
    }

    "have exit code 1 when the --path is missing both args " in {
      MetaDataValidatorCommandLineApp.run(Array("--path", basePath + "metaData.csv", basePath + "schema.txt"))._2 mustEqual 1
    }


    "have exit code 2 when the schema is invalid" in {
      MetaDataValidatorCommandLineApp.run(Array(basePath + "metaData.csv", basePath + "badSchema.txt"))._2 mustEqual 2
    }

    "have exit code 3 when the metadata is invalid" in {
      MetaDataValidatorCommandLineApp.run(Array(basePath + "acceptance/standardRulesFailMetaData.csv", basePath + "acceptance/standardRulesSchema.txt"))._2 mustEqual 3
    }

  }

  "Parsing schema" should {
    val app = new MetaDataValidatorApp with AllErrorsMetaDataValidator { val pathSubstitutions = List[(String,String)]() }

    "report position on parse fail" in {

      val schema =
        """version 1.0
          |@totalColumns 1
          |Name: regox("A")
        """.stripMargin

      app.parseAndValidate(new StringReader(schema)) must beLike {
        case Failure(msgs) => msgs.list mustEqual List(SchemaMessage(
          """[3.7] failure: Invalid schema text
            |
            |Name: regox("A")
            |
            |      ^""".stripMargin))
      }
    }
  }

  "Validation" should {
    val app = new MetaDataValidatorApp with AllErrorsMetaDataValidator { val pathSubstitutions = List[(String,String)]() }

    def parse(filePath: String): Schema = app.parseSchema(filePath) fold (f => throw new IllegalArgumentException(f.toString()), s => s)

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

