package uk.gov.tna.dri.validator

import org.specs2.mutable.Specification
import uk.gov.tna.dri.schema._
import java.io.{Reader, StringReader}
import scalaz.Success
import uk.gov.tna.dri.schema.Schema
import scalaz.Failure

class MetaDataValidatorSpec extends Specification {

  implicit def stringToStringReader(s: String): StringReader = new StringReader(s.replaceAll("\n\\s+", "\n"))

  implicit def stringToSchema(s: String): Schema = {
    val schemaParser = new SchemaParser() {
      override def parse(reader: Reader): ParseResult[Schema] = {
        super.parse(reader) match {
          case s @ Success(schema: Schema, _) => s
          case NoSuccess(message, next) => throw new RuntimeException(message)
        }
      }
    }

    schemaParser.parse(s).get
  }

  object TestMetaDataValidator extends AllErrorsMetaDataValidator

  import TestMetaDataValidator._

  "Validation" should {

    "succeed for correct total columns for multiple lines" in {
      val schema =
        """@totalColumns 2 @noHeader
           column1:
           column2:
        """

      val metaData =
        """col1, col2
           col1, col2
        """

      validate(metaData, schema) must beLike { case Success(_) => ok }
    }

    "fail for incorrect number of total columns for multiple lines" in {
      val schema =
        """@totalColumns 3 @noHeader
           column1:
           column2:
           column3:
        """

      val metaData =
        """col1, col2, col3
           col1, col2
           col1, col2, col3
        """

      validate(metaData, schema) must beLike {
        case Failure(messages) => messages.list mustEqual List("Expected @totalColumns of 3 and found 2 on line 2", "Missing value at line: 2, column: column3")
      }
    }

    "fail if columns on multiple rows do not pass" in {
      val schema =
        """@totalColumns 2 @noHeader
           first: regex("[3-8]*")
           second: regex("[a-c]*")
        """

      val metaData =
        """34,xxxy
           abcd,uii
        """

      validate(metaData, schema) must beLike {
        case Failure(messages) => messages.list mustEqual List (
          """regex("[a-c]*") fails for line: 1, column: second, value: xxxy""",
          """regex("[3-8]*") fails for line: 2, column: first, value: abcd""",
          """regex("[a-c]*") fails for line: 2, column: second, value: uii""")
      }
    }

    "succeed for multiple rows" in {
      val schema =
        """@totalColumns 2 @noHeader
           col1:
           col2WithRule: regex("[0-9]*")
        """

      val metaData =
        """someData,345
           someMore,12
        """

      validate(metaData, schema) must beLike { case Success(_) => ok }
    }

    "fail for a single rule" in {
      val schema =
        """@totalColumns 2 @noHeader
           Col1: regex("C11")
           Col2:
        """

      val metaData = "c11,c12"

      validate(metaData, schema) should beLike {
        case Failure(messages) => messages.list mustEqual List("""regex("C11") fails for line: 1, column: Col1, value: c11""")
      }
    }

    "fail for rule when cell missing" in {
      val schema =
        """@totalColumns 2 @noHeader
           Col1:
           Col2: regex("[0-9]")
        """

      val metaData = "1"

      validate(metaData, schema) should beLike {
        case Failure(messages) => messages.list mustEqual List("Expected @totalColumns of 2 and found 1 on line 1", "Missing value at line: 1, column: Col2")
      }
    }

    "succeed for more than one regex" in {
      val schema =
        """@totalColumns 1 @noHeader
           c1: regex("\w+") regex("^S.+")
        """

      val metaData = """Scooby"""

      validate(metaData, schema) must beLike { case Success(_) => ok }
    }

    "fail when at least one regex fails for multiple regex provided in a column definition" in {
      val schema =
        """@totalColumns 1 @noHeader
           c1: regex("\w+") regex("^T.+") regex("^X.+")
        """

      val metaData = """Scooby"""

      validate(metaData, schema) must beLike {
        case Failure(messages) => messages.list mustEqual List("""regex("^T.+") fails for line: 1, column: c1, value: Scooby""", """regex("^X.+") fails for line: 1, column: c1, value: Scooby""")
      }
    }

    "succeed for multiple rows with InRule as string literal" in {
      val schema =
        """@totalColumns 2 @noHeader
           col1:
           col2WithRule: regex("[0-9a-z]*") in("345dog")
        """

      val metaData =
        """someData,dog
           someMore,dog
        """

      validate(metaData, schema) must beLike { case Success(_) => ok }
    }

    "succeed for InRule as column reference" in {
      val schema =
        """@totalColumns 2 @noHeader
           col1:
           col2WithRule: regex("\w*") in($col1)
        """

      val metaData =
        """blah_mustBeIn_blah,mustBeIn
           blah_andMustBeIn,andMustBeIn"""

      validate(metaData, schema) must beLike { case Success(_) => ok }
    }

    "fail for InRule as column reference where case does not match" in {
      val schema =
        """@totalColumns 2 @noHeader
           col1:
           col2WithRule: regex("\w*") in($col1)
        """

      val metaData =
        """blah_MUSTBEIN_blah,mustBeIn
           blah_andMustBeIn,andMustBeIn"""

      validate(metaData, schema) should beLike {
        case Failure(messages) => messages.list mustEqual List("""in($col1) fails for line: 1, column: col2WithRule, value: mustBeIn""")
      }
    }

    "succeed for InRule as column reference where case is ignored" in {
      val schema =
        """@totalColumns 2 @noHeader
           col1:
           col2WithRule: regex("\w*") in($col1) @ignoreCase
        """

      val metaData =
        """blah_MUSTBEIN_blah,mustBeIn
           blah_andMustBeIn,andMustBeIn"""

      validate(metaData, schema) must beLike { case Success(_) => ok }
    }

    "succeed when @optional is given for an empty cell" in {
      val schema =
        """@totalColumns 3 @noHeader
           Col1:
           Col2: regex("[0-9]") @optional
           Col3:
        """

      val metaData = "1, , 3"

      validate(metaData, schema) must beLike { case Success(_) => ok }
    }

    "fail when @optional is given for non empty cell with a failing rule" in {
      val schema =
        """@totalColumns 3 @noHeader
           Col1:
           Col2: regex("[0-9]") @optional
           Col3:
        """

      val metaData = "1,a,3"

      validate(metaData, schema) should beLike {
        case Failure(messages) => messages.list mustEqual List("""regex("[0-9]") fails for line: 1, column: Col2, value: a""")
      }
    }

    "pass for empty cell with @optional but fail for empty cell without @optional for the same rule" in {
      val schema =
        """@totalColumns 4 @noHeader
           Col1:
           Col2: regex("[0-9]") @optional
           Col3: regex("[0-9]")
           Col4:
        """

      val metaData =
        """1,,,4
           1,a,,4
        """

      validate(metaData, schema) should beLike {
        case Failure(messages) => messages.list mustEqual List(
          """regex("[0-9]") fails for line: 1, column: Col3, value: """,
          """regex("[0-9]") fails for line: 2, column: Col2, value: a""",
          """regex("[0-9]") fails for line: 2, column: Col3, value: """)
      }
    }

    "ignore case of a given regex" in {
      val schema =
        """@totalColumns 1 @noHeader
           1: regex("[a-z]+") @ignoreCase
        """

      val metaData = """SCOOBY"""

      validate(metaData, schema) must beLike { case Success(_) => ok }
    }

    "ignore case with in rule succeeds if value contains regex characters" in {
      val schema =
        """@totalColumns 1 @noHeader
           1: in("[abc]") @ignoreCase
        """

      val metaData ="""[Abc"""

      validate(metaData, schema) must beLike { case Success(_) => ok }
    }

    "fail to ignore case of a given regex when not providing @ignoreCase" in {
      val schema =
        """@totalColumns 1 @noHeader
           Col1: regex("[a-z]+")
        """

      val metaData = "SCOOBY"

      validate(metaData, schema) should beLike {
        case Failure(messages) => messages.list mustEqual List("""regex("[a-z]+") fails for line: 1, column: Col1, value: SCOOBY""")
      }
    }

    "succeed with valid file path" in {
      val schema =
        """@totalColumns 1 @noHeader
           1: fileExists
        """

      val metaData = "src/test/resources/uk/gov/tna/dri/schema/mustExistForRule.txt"

      validate(metaData, schema) must beLike { case Success(_) => ok }
    }

    "succeed with valid file path where fileExists rule prepends root path to the filename" in {
      val schema =
        """@totalColumns 1 @noHeader
           1: fileExists("src/test/resources/uk/gov/")
        """

      val metaData = "tna/dri/schema/mustExistForRule.txt"

      validate(metaData, schema) must beLike { case Success(_) => ok }
    }

    "fail for non existent file path" in {
      val schema =
        """@totalColumns 1 @noHeader
           FirstColumn: fileExists
        """

      val metaData = "some/non/existent/file"

      validate(metaData, schema) must beLike {
        case Failure(messages) => messages.list mustEqual List("fileExists fails for line: 1, column: FirstColumn, value: some/non/existent/file")
      }
    }

    "fail when first line contains invalid data and noHeader directive is set" in {
      val schema =
        """@totalColumns 2 @noHeader
           col1:
           col2WithRule: regex("[0-9a-z]*") in("dog")
        """

      val metaData =
        """someData,thisisrubbish
           someMore,dog
        """

      validate(metaData, schema) must beLike {
        case Failure(messages) => messages.list mustEqual List("""in("dog") fails for line: 1, column: col2WithRule, value: thisisrubbish""")
      }
    }

    "fail when first line contains invalid data and noHeader directive is not set" in {
      val schema =
        """@totalColumns 2
           col1:
           col2WithRule: regex("[0-9a-z]*") in("dog")
        """

      val metaData =
        """someData,this line is skipped because no header is not set i.e there is a header (this line) to skip
           someMore,thisisrubbish
        """

      validate(metaData, schema) must beLike {
        case Failure(messages) => messages.list mustEqual List("""in("dog") fails for line: 1, column: col2WithRule, value: thisisrubbish""")
      }
    }

    "succeed when either side of or rule passes" in {
      val schema =
        """@totalColumns 1 @noHeader
           ThisOrThat: in("This") or in("That")
        """

      val metaData =
        """This
           That
        """

      validate(metaData, schema) must beLike { case Success(_) => ok }
    }

    "fail when neither side of or rule passes" in {
      val schema =
        """@totalColumns 1 @noHeader
           ThisOrThat: in("This") or in("That")
        """

      val metaData = "SomethingElse"

      validate(metaData, schema) must beLike {
        case Failure(messages) => messages.list mustEqual List("""in("This") or in("That") fails for line: 1, column: ThisOrThat, value: SomethingElse""")
      }
    }

    "succeed when one of 3 'or' rules passes" in {
      val schema =
        """@totalColumns 1 @noHeader
           Red: regex("\d+") or regex("R.*") or in("red")
        """

      val metaData =
        """Red
           red
        """

      validate(metaData, schema) must beLike { case Success(_) => ok }
    }

    "succeed for 'is' rule" in {
      val schema =
        """@totalColumns 1 @noHeader
           Country: is("UK")
        """

      val metaData = "UK"

      validate(metaData, schema) must beLike { case Success(_) => ok }
    }

    "succeed for 'is' cross reference rule" in {
      val schema =
        """@totalColumns 2 @noHeader
           Country: is($MyCountry)
           MyCountry:
        """

      val metaData = "United Kingdom,United Kingdom"

      validate(metaData, schema) must beLike { case Success(_) => ok }
    }

    "succeed for 2 'is' rule" in {
      val schema =
        """@totalColumns 1 @noHeader
           Country: is("UK") is("uk") @ignoreCase
        """

      val metaData = "UK"

      validate(metaData, schema) must beLike { case Success(_) => ok }
    }

    "fail for 'is' rule that is not matched" in {
      val schema =
        """@totalColumns 1 @noHeader
           Country: is("France")
        """

      val metaData = "UK"

      validate(metaData, schema) must beLike {
        case Failure(messages) => messages.list mustEqual List("""is("France") fails for line: 1, column: Country, value: UK""")
      }
    }

    "fail for 'is' cross reference rule that is not matched" in {
      val schema =
        """@totalColumns 2 @noHeader
           Country: is($MyCountry)
           MyCountry:
        """

      val metaData = "United,United Kingdom"

      validate(metaData, schema) must beLike {
        case Failure(messages) => messages.list mustEqual List("""is($MyCountry) fails for line: 1, column: Country, value: United""")
      }
    }

    "succeed for 'isNot' rule" in {
      val schema =
        """@totalColumns 1 @noHeader
           Country: isNot("United States")
        """

      val metaData = "United Kingdom"

      validate(metaData, schema) must beLike { case Success(_) => ok }
    }

    "succeed for 'isNot' cross reference rule" in {
      val schema =
        """@totalColumns 2 @noHeader
           Country: isNot($MyCountry)
           MyCountry:
        """

      val metaData = "United Kingdom,United States"

      validate(metaData, schema) must beLike { case Success(_) => ok }
    }

    "succeed for 2 'isNot' rule" in {
      val schema =
        """@totalColumns 1 @noHeader
           Country: isNot("United States") isNot("Kingdom") @ignoreCase
        """

      val metaData = "United Kingdom"

      validate(metaData, schema) must beLike { case Success(_) => ok }
    }

    "fail for 'isNot' rule that is not matched" in {
      val schema =
        """@totalColumns 1 @noHeader
           Country: isNot("United Kingdom")
        """

      val metaData = "United Kingdom"

      validate(metaData, schema) must beLike {
        case Failure(messages) => messages.list mustEqual List("""isNot("United Kingdom") fails for line: 1, column: Country, value: United Kingdom""")
      }
    }

    "fail for 'isNot' cross reference rule that is not matched" in {
      val schema =
        """@totalColumns 2 @noHeader
           Country: isNot($MyCountry)
           MyCountry:
        """

      val metaData = "United Kingdom,United Kingdom"

      validate(metaData, schema) must beLike {
        case Failure(messages) => messages.list mustEqual List("""isNot($MyCountry) fails for line: 1, column: Country, value: United Kingdom""")
      }
    }

    "succeed for 'starts' rule" in {
      val schema =
        """@totalColumns 1 @noHeader
           Country: starts("United")
        """

      val metaData = "United Kingdom"

      validate(metaData, schema) must beLike { case Success(_) => ok }
    }

    "succeed for 'starts' cross reference rule" in {
      val schema =
        """@totalColumns 2 @noHeader
           Country: starts($MyCountry)
           MyCountry:
        """

      val metaData = "United Kingdom,United"

      validate(metaData, schema) must beLike { case Success(_) => ok }
    }

    "succeed for 2 'starts' rule" in {
      val schema =
        """@totalColumns 1 @noHeader
           Country: starts("United") starts("UNITED") @ignoreCase
        """

      val metaData = "United Kingdom"

      validate(metaData, schema) must beLike { case Success(_) => ok }
    }

    "fail for 'starts' rule that is not matched" in {
      val schema =
        """@totalColumns 1 @noHeader
           Country: starts("united")
        """

      val metaData = "United Kingdom"

      validate(metaData, schema) must beLike {
        case Failure(messages) => messages.list mustEqual List("""starts("united") fails for line: 1, column: Country, value: United Kingdom""")
      }
    }

    "fail for 'starts' cross reference rule that is not matched" in {
      val schema =
        """@totalColumns 2 @noHeader
           Country: starts($MyCountry)
           MyCountry:
        """

      val metaData = "United,United Kingdom"

      validate(metaData, schema) must beLike {
        case Failure(messages) => messages.list mustEqual List("""starts($MyCountry) fails for line: 1, column: Country, value: United""")
      }
    }

    "succeed for 'ends' rule" in {
      val schema =
        """@totalColumns 1 @noHeader
           Country: ends("Kingdom")
        """

      val metaData = "United Kingdom"

      validate(metaData, schema) must beLike { case Success(_) => ok }
    }

    "succeed for 'ends' cross reference rule" in {
      val schema =
        """@totalColumns 2 @noHeader
           Country: ends($MyCountry)
           MyCountry:
        """

      val metaData = "United Kingdom,Kingdom"

      validate(metaData, schema) must beLike { case Success(_) => ok }
    }

    "succeed for 2 'ends' rule" in {
      val schema =
        """@totalColumns 1 @noHeader
           Country: ends("Kingdom") ends("KINGDOM") @ignoreCase
        """

      val metaData = "United Kingdom"

      validate(metaData, schema) must beLike { case Success(_) => ok }
    }

    "fail for 'ends' rule that is not matched" in {
      val schema =
        """@totalColumns 1 @noHeader
           Country: ends("kingdom")"""

      val metaData = "United Kingdom"

      validate(metaData, schema) must beLike {
        case Failure(messages) => messages.list mustEqual List("""ends("kingdom") fails for line: 1, column: Country, value: United Kingdom""")
      }
    }

    "fail for 'ends' cross reference rule that is not matched" in {
      val schema =
        """@totalColumns 2 @noHeader
           Country: ends($MyCountry)
           MyCountry:
        """

      val metaData = "United Kingdom,States"

      validate(metaData, schema) must beLike {
        case Failure(messages) => messages.list mustEqual List("""ends($MyCountry) fails for line: 1, column: Country, value: United Kingdom""")
      }
    }

    "fail for multiple duplicates for unique rule" in {

      val schema =
        """@totalColumns 1 @noHeader
           Name: unique
        """

      val metaData =
        """Bob
           Jim
           Ben
           Jim
           Jim
        """

      validate(metaData, schema) must beLike {
        case Failure(messages) => messages.list mustEqual List("unique fails for line: 4, column: Name, value: Jim (original at line: 2)","unique fails for line: 5, column: Name, value: Jim (original at line: 2)")
      }
    }
  }
}