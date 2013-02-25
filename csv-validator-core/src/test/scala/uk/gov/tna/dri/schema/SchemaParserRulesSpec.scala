package uk.gov.tna.dri.schema

import org.specs2.mutable._
import org.specs2.matcher.ParserMatchers
import java.io.StringReader

class SchemaParserRulesSpec extends Specification with ParserMatchers {

  object TestSchemaParser extends SchemaParser

  override val parsers = TestSchemaParser

  import TestSchemaParser._

  val globalDirsOne = List(TotalColumns(1))
  val globalDirsTwo = List(TotalColumns(2))

  "Schema" should {

    "succeed for valid regex rule" in {
      val schema = """@totalColumns 1
                      LastName: regex("[a]")"""

      parse(new StringReader(schema)) must beLike { case Success(Schema(globalDirsOne, List(ColumnDefinition("LastName", List(RegexRule(Literal(Some(r)))), _))), _) => r mustEqual "[a]" }
    }

    "fail for an invalid regex" in {
      val schema = """@totalColumns 1
                      Something: regex ("[0-9")"""

      parse(new StringReader(schema)) must beLike { case Failure(message, _) => (message mustEqual """regex invalid: ("[0-9")""") }
    }

    "fail for missing quotes defining a regex" in {
      val schema = """@totalColumns 3
                      LastName:
                      FirstName: regex ("a)
                      Age:"""

      parse(new StringReader(schema)) must beLike {
        case Failure(message, _) => message mustEqual """regex not correctly delimited as ("your regex")"""
      }
    }

    "fail for missing value in regex" in {
      val schema = """@totalColumns 1
                      Something: regex"""

      parse(new StringReader(schema)) must beLike { case Failure(message, _) => (message mustEqual "regex not correctly delimited as (\"your regex\")") }
    }

    "succeed for more than 1 regex rule" in {
      val schema = """@totalColumns 1
                      LastName: regex("[a]") regex("[0-5]")"""

      parse(new StringReader(schema)) must beLike { case Success(_, _) => ok }
    }

    "succeed for file exists rule" in {
      val schema = """@totalColumns 1
                      Name: fileExists"""

      parse(new StringReader(schema)) must beLike { case Success(Schema(globalDirsOne, List(ColumnDefinition("Name", List(FileExistsRule(Literal(None))), _))), _) => ok}
    }

    "fail for file exists rule with empty ()" in {
      val schema = """@totalColumns 1
                      Name: fileExists()"""

      parse(new StringReader(schema)) must beLike { case f@Failure("fileExists rule has an invalid file path", _) => ok}
    }

    "succeed for file exists rule with root file path" in {
      val schema = """@totalColumns 1
                      Name: fileExists("some/root/path")"""

      parse(new StringReader(schema)) must beLike {
        case Success(Schema(globalDirsOne, List(ColumnDefinition("Name", List(FileExistsRule(Literal(Some(rootPath)))), _))), _) => {
          rootPath mustEqual "some/root/path"
        }
      }
    }

    "fail for non quoted root file path" in {
      val schema = """@totalColumns 1
                      Name: fileExists(some/other/root/path)"""

      parse(new StringReader(schema)) must beLike { case Failure("fileExists rule has an invalid file path", _) => ok}
    }

    "fail for non parentheses" in {
      val schema = """@totalColumns 1
                      Name: fileExists /root/path"""

      parse(new StringReader(schema)) must beLike { case Failure("Column definition contains invalid text", _) => ok}
    }

    "succeed for or rule" in {
      val schema =
        """@totalColumns 1
           Country: in("UK") or in("England")"""

      parse(new StringReader(schema)) must beLike {
        case Success(Schema(globalDirsOne, List(ColumnDefinition("Country", List( OrRule( InRule(Literal(Some("UK"))), InRule(Literal(Some("England"))))), _))), _) => ok
      }
    }

    "fail for or rule with no lhs" in {
      val schema =
        """@totalColumns 1
           Country: or in("England")"""

      parse(new StringReader(schema)) must beLike { case Failure("Column definition contains invalid text", _) => ok }
    }

    "fail for or rule with no rhs" in {
      val schema =
        """@totalColumns 1
           Country: in("UK") or"""

      parse(new StringReader(schema)) must beLike { case Failure("Invalid rule", _) => ok }
    }

    "fail for or rule with no lhs or rhs" in {
      val schema =
        """@totalColumns 1
           Country: or"""

      parse(new StringReader(schema)) must beLike { case Failure("Column definition contains invalid text", _) => ok }
    }

    "succeed for two 'or' rules" in {
      val schema =
        """@totalColumns 1
           Country: in("UK") or in("England") or in("France")"""

      parse(new StringReader(schema)) must beLike {
        case Success(Schema(globalDirsOne, List(ColumnDefinition("Country",
                                                List(OrRule(InRule(Literal(Some("UK"))), OrRule(InRule(Literal(Some("England"))), InRule(Literal(Some("France")))))  ), _))), _) => ok
      }
    }

    "succeed for all no argument rules with hard coded expressions" in {
      val schema =
        """@totalColumns 1
           Country: uri xDateTime xDate ukDate xTime uuid4 positiveInteger"""

      parse(new StringReader(schema)) must beLike {
        case Success(Schema(globalDirsOne, List(ColumnDefinition("Country", List( UriRule(),XsdDateTimeRule(),XsdDateRule(),UkDateRule(),XsdTimeRule(),Uuid4Rule(),PositiveIntegerRule()), _))), _) => ok
      }
    }

    "succeed for unique rule" in {
      val schema =
        """@totalColumns 1
           Country: unique"""

      parse(new StringReader(schema)) must beLike {
        case Success(Schema(_, List(ColumnDefinition("Country", List(UniqueRule()), _))), _) => ok
      }
    }

    "succeed for checksum rule" in {
      val schema =
        """@totalColumns 1
           FileChecksum: checksum("myFile.txt","MD5")"""

      parse(new StringReader(schema)) must beLike {
        case Success(Schema(_, List(ColumnDefinition("FileChecksum", List(ChecksumRule(Literal(Some("myFile.txt")), "MD5")), _))), _) => ok
      }
    }
  }
}