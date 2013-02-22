package uk.gov.tna.dri.validator

import org.specs2.mutable.Specification
import uk.gov.tna.dri.schema._
import java.io.StringReader
import scalaz.Success
import uk.gov.tna.dri.schema.ColumnDefinition
import uk.gov.tna.dri.schema.RegexRule
import scalaz.Failure
import uk.gov.tna.dri.schema.Schema

class MetaDataValidatorSpec extends Specification {

  val schemaParser = new SchemaParser {}

  object TestMetaDataValidator extends AllErrorsMetaDataValidator

  import TestMetaDataValidator._

  "Validation" should {
    val globalDirsOne = List(TotalColumns(1), NoHeader())
    val globalDirsTwo = List(TotalColumns(2), NoHeader())
    val globalDirsThree = List(TotalColumns(3), NoHeader())
    val globalDirsFour = List(TotalColumns(4), NoHeader())

    "succeed for correct total columns for multiple lines" in {
      val metaData =
        """col1, col2
           col1, col2"""

      val columnDefinitions = List(new ColumnDefinition("\"column1\""),new ColumnDefinition("\"column2\""))
      validate(new StringReader(metaData), Schema(globalDirsTwo, columnDefinitions)) must beLike { case Success(_) => ok }
    }

    "fail for incorrect number of total columns for multiple lines" in {
      val metaData =
        """col1, col2, col3
           col1, col2
           col1, col2, col3"""

      val columnDefinitions = List(new ColumnDefinition("\"column1\""),new ColumnDefinition("\"column2\""),new ColumnDefinition("\"column3\""))

      validate(new StringReader(metaData), Schema(globalDirsThree, columnDefinitions)) must beLike {
        case Failure(messages) => messages.list mustEqual List("Expected @totalColumns of 3 and found 2 on line 2", "Missing value at line: 2, column: \"column3\"")
      }
    }

    "fail if columns on multiple rows do not pass" in {
      val schema = Schema(globalDirsTwo, List(ColumnDefinition("first", List(RegexRule(Literal(Some("[3-8]*"))))), ColumnDefinition("second",List(RegexRule(Literal(Some("[a-c]*")))))))

      val metaData =
        """34,xxxy
           |abcd,uii""".stripMargin

      validate(new StringReader(metaData), schema) must beLike {
        case Failure(messages) => messages.list mustEqual List (
          """regex("[a-c]*") fails for line: 1, column: second, value: xxxy""",
          """regex("[3-8]*") fails for line: 2, column: first, value: abcd""",
          """regex("[a-c]*") fails for line: 2, column: second, value: uii""")
      }
    }

    "succeed for multiple rows" in {
      val schema = Schema(globalDirsTwo, List(ColumnDefinition("col1"), ColumnDefinition("col2WithRule", List(RegexRule(Literal(Some("[0-9]*")))))))

      val metaData =
        """someData,345
           someMore,12"""

      validate(new StringReader(metaData), schema) must beLike { case Success(_) => ok }
    }

    "fail for a single rule" in {
      val m = "c11,c12"
      val schema = Schema(globalDirsTwo, List(ColumnDefinition("Col1", List(RegexRule(Literal(Some("C11"))))), ColumnDefinition("Col2")))

      validate(new StringReader(m), schema) should beLike {
        case Failure(messages) => messages.list mustEqual List("""regex("C11") fails for line: 1, column: Col1, value: c11""")
      }
    }

    "fail for rule when cell missing" in {
      val m = "1"
      val schema = Schema(globalDirsTwo, List(ColumnDefinition("Col1"), ColumnDefinition("Col2", List(RegexRule(Literal(Some("[0-9]")))))))

      validate(new StringReader(m), schema) should beLike {
        case Failure(messages) => messages.list mustEqual List("Expected @totalColumns of 2 and found 1 on line 1", "Missing value at line: 1, column: Col2")
      }
    }

    "succeed for more than one regex" in {
      val columnDefinitions = ColumnDefinition("c1", List(RegexRule(Literal(Some("\\w+"))), RegexRule(Literal(Some("^S.+"))))) :: Nil
      val schema = Schema(globalDirsOne, columnDefinitions)

      val metaData = """Scooby"""

      validate(new StringReader(metaData), schema) must beLike { case Success(_) => ok }
    }

    "fail when at least one regex fails for multiple regex provided in a column definition" in {
      val columnDefinitions = ColumnDefinition("c1", List(RegexRule(Literal(Some("\\w+"))), RegexRule(Literal(Some("^T.+"))), RegexRule(Literal(Some("^X.+"))))) :: Nil
      val schema = Schema(globalDirsOne, columnDefinitions)

      val metaData = """Scooby"""

      validate(new StringReader(metaData), schema) must beLike {
        case Failure(messages) => messages.list mustEqual List("""regex("^T.+") fails for line: 1, column: c1, value: Scooby""", """regex("^X.+") fails for line: 1, column: c1, value: Scooby""")
      }
    }

    "succeed for multiple rows with InRule as string literal" in {
      val columnDefinitions = ColumnDefinition("col1") :: ColumnDefinition("col2WithRule", List(RegexRule(Literal(Some("[0-9a-z]*"))), InRule(Literal(Some("345dog"))))) :: Nil
      val schema = Schema(globalDirsTwo, columnDefinitions)

      val metaData =
        """someData,dog
           someMore,dog"""

      validate(new StringReader(metaData), schema) must beLike { case Success(_) => ok }
    }

    "succeed for InRule as column reference" in {
      val columnDefinitions = ColumnDefinition("col1") :: ColumnDefinition("col2WithRule", List(RegexRule(Literal(Some("\\w*"))), InRule(ColumnReference("col1")))) :: Nil
      val schema = Schema(globalDirsTwo, columnDefinitions)

      val metaData =
        """blah_mustBeIn_blah,mustBeIn
           blah_andMustBeIn,andMustBeIn"""

      validate(new StringReader(metaData), schema) must beLike { case Success(_) => ok }
    }

    "fail for InRule as column reference where case does not match" in {
      val columnDefinitions = ColumnDefinition("col1") :: ColumnDefinition("col2WithRule", List(RegexRule(Literal(Some("\\w*"))), InRule(ColumnReference("col1")))) :: Nil
      val schema = Schema(globalDirsTwo, columnDefinitions)

      val metaData =
        """blah_MUSTBEIN_blah,mustBeIn
           blah_andMustBeIn,andMustBeIn"""

      validate(new StringReader(metaData), schema) should beLike {
        case Failure(messages) => messages.list mustEqual List("""in($col1) fails for line: 1, column: col2WithRule, value: mustBeIn""")
      }
    }

    "succeed for InRule as column reference where case is ignored" in {
      val columnDefinitions = ColumnDefinition("col1") :: ColumnDefinition("col2WithRule", List(RegexRule(Literal(Some("\\w*"))), InRule(ColumnReference("col1"))), List(IgnoreCase())) :: Nil
      val schema = Schema(globalDirsTwo, columnDefinitions)

      val metaData =
        """blah_MUSTBEIN_blah,mustBeIn
           blah_andMustBeIn,andMustBeIn"""

      validate(new StringReader(metaData), schema) must beLike { case Success(_) => ok }
    }

    "succeed when @optional is given for an empty cell" in {

      val columnDefinitions = ColumnDefinition("Col1") ::  ColumnDefinition("Col2", List(RegexRule(Literal(Some("[0-9]")))), List(Optional())) :: ColumnDefinition("Col3") :: Nil
      val schema = Schema(globalDirsThree, columnDefinitions)
      val m = "1, , 3"

      validate(new StringReader(m), schema) must beLike { case Success(_) => ok }
    }

    "fail when @optional is given for non empty cell with a failing rule" in {

      val columnDefinitions = ColumnDefinition("Col1") ::  ColumnDefinition("Col2", List(RegexRule(Literal(Some("[0-9]")))), List(Optional())) :: ColumnDefinition("Col3") :: Nil
      val schema = Schema(globalDirsThree, columnDefinitions)
      val m = "1,a,3"

      validate(new StringReader(m), schema) should beLike {
        case Failure(messages) => messages.list mustEqual List("""regex("[0-9]") fails for line: 1, column: Col2, value: a""")
      }
    }

    "pass for empty cell with @optional but fail for empty cell without @optional for the same rule" in {

      val columnDefinitions = ColumnDefinition("Col1") :: ColumnDefinition("Col2", List(RegexRule(Literal(Some("[0-9]")))), List(Optional())) :: ColumnDefinition("Col3", List(RegexRule(Literal(Some("[0-9]"))))) :: ColumnDefinition("Col4") :: Nil
      val schema = Schema(globalDirsFour, columnDefinitions)

      val m =
        """1,,,4
          |1,a,,4""".stripMargin

      validate(new StringReader(m), schema) should beLike {
        case Failure(messages) => messages.list mustEqual List(
          """regex("[0-9]") fails for line: 1, column: Col3, value: """,
          """regex("[0-9]") fails for line: 2, column: Col2, value: a""",
          """regex("[0-9]") fails for line: 2, column: Col3, value: """)
      }
    }

    "ignore case of a given regex" in {
      val columnDefinitions = ColumnDefinition("1", List(RegexRule(Literal(Some("[a-z]+")))), List(IgnoreCase())) :: Nil
      val schema = Schema(globalDirsOne, columnDefinitions)
      val meta = """SCOOBY"""

      validate(new StringReader(meta), schema) must beLike { case Success(_) => ok }
    }

    "ignore case with in rule succeeds if value contains reg ex characters" in {
      val columnDefinitions = ColumnDefinition("1", List(InRule(Literal(Some("[abc]")))), List(IgnoreCase())) :: Nil
      val schema = Schema(globalDirsOne, columnDefinitions)
      val meta ="""[abc"""
      validate(new StringReader(meta), schema) must beLike { case Success(_) => ok }
    }

    "fail to ignore case of a given regex when not providing @ignoreCase" in {
      val columnDefinitions = ColumnDefinition("Col1", List(RegexRule(Literal(Some("[a-z]+"))))) :: Nil
      val schema = Schema(globalDirsOne, columnDefinitions)
      val meta = "SCOOBY"

      validate(new StringReader(meta), schema) should beLike {
        case Failure(messages) => messages.list mustEqual List("""regex("[a-z]+") fails for line: 1, column: Col1, value: SCOOBY""")
      }
    }

    "succeed with valid file path" in {
      val columnDefinitions = ColumnDefinition("1", List(FileExistsRule())) :: Nil
      val schema = Schema(globalDirsOne, columnDefinitions)
      val meta = "src/test/resources/uk/gov/tna/dri/schema/mustExistForRule.txt"

      validate(new StringReader(meta), schema) must beLike { case Success(_) => ok }
    }

    "succeed with valid file path where fileExists rule prepends root path to the filename" in {
      val columnDefinitions = ColumnDefinition("1", List(FileExistsRule(Literal(Some("src/test/resources/uk/gov/"))))) :: Nil
      val schema = Schema(globalDirsOne, columnDefinitions)
      val meta = "tna/dri/schema/mustExistForRule.txt"

      validate(new StringReader(meta), schema) must beLike { case Success(_) => ok }
    }

    "fail for non existent file path" in {
      val columnDefinitions = ColumnDefinition("First Column", List(FileExistsRule())) :: Nil
      val schema = Schema(globalDirsOne, columnDefinitions)
      val meta = "some/non/existent/file"

      validate(new StringReader(meta), schema) must beLike {
        case Failure(messages) => messages.list mustEqual List("fileExists fails for line: 1, column: First Column, value: some/non/existent/file")
      }
    }

    "fail when first line contains invalid data and noHeader directive is set" in {
      val columnDefinitions = ColumnDefinition("col1") :: ColumnDefinition("col2WithRule", List(RegexRule(Literal(Some("[0-9a-z]*"))), InRule(Literal(Some("345dog"))))) :: Nil
      val schema = Schema(globalDirsTwo, columnDefinitions)
      val metaData =
        """someData,thisisrubbish
           someMore,dog"""

      validate(new StringReader(metaData), schema) must beLike {
        case Failure(messages) => messages.list mustEqual List("""in("345dog") fails for line: 1, column: col2WithRule, value: thisisrubbish""")
      }
    }

    "succeed when first line contains invalid data and noHeader directive is missing" in {
      val columnDefinitions = ColumnDefinition("col1") ::
                              ColumnDefinition("col2WithRule", List(RegexRule(Literal(Some("[0-9a-z]*"))), InRule(Literal(Some("dog"))))) :: Nil

      val globalDirsTwoNoheader = List(TotalColumns(2), NoHeader())
      val schema = Schema(globalDirsTwoNoheader, columnDefinitions)

      val metaData =
        """someData,dog
           someMore,dog"""

      validate(new StringReader(metaData), schema) must beLike { case Success(_) => ok }
    }

    "succeed when either side of or rule passes" in {
      val orRule = OrRule(InRule(Literal(Some("This"))), InRule(Literal(Some("That"))))
      val columnDefinitions = ColumnDefinition("ThisOrThat", List(orRule)) :: Nil
      val schema = Schema(List(TotalColumns(1), NoHeader()), columnDefinitions)

      val metaData = """This
                       |That""".stripMargin

      validate(new StringReader(metaData), schema) must beLike { case Success(_) => ok }
    }

    "fail when neither side of or rule passes" in {
      val orRule = OrRule(InRule(Literal(Some("This"))), InRule(Literal(Some("That"))))
      val columnDefinitions = ColumnDefinition("ThisOrThat", List(orRule)) :: Nil
      val schema = Schema(List(TotalColumns(1), NoHeader()), columnDefinitions)
      val metaData = "SomethingElse"

      validate(new StringReader(metaData), schema) must beLike {
        case Failure(messages) => messages.list mustEqual List("""in("This") or in("That") fails for line: 1, column: ThisOrThat, value: SomethingElse""")
      }
    }

    "succeed when one of 3 'or' rules passes" in {
      val orRule = OrRule(RegexRule(Literal(Some("[A-Z]+"))), OrRule(RegexRule(Literal(Some("R.*"))), InRule(Literal(Some("red")))))
      val columnDefinitions = ColumnDefinition("Red", List(orRule)) :: Nil
      val schema = Schema(List(TotalColumns(1), NoHeader()), columnDefinitions)

      val metaData = """Red
                       |red""".stripMargin

      validate(new StringReader(metaData), schema) must beLike { case Success(_) => ok }
    }

    "succeed for 'is' rule" in {
      val schema = Schema(List(TotalColumns(1), NoHeader()),
                          List(ColumnDefinition("Country", List(IsRule(Literal(Some("UK")))))))

      val metaData = "UK"

      validate(new StringReader(metaData), schema) must beLike { case Success(_) => ok }
    }

    "succeed for 'is' cross reference rule" in {
      val schema = Schema(List(TotalColumns(2), NoHeader()),
                          List(ColumnDefinition("Country", List(IsRule(ColumnReference("MyCountry")))),
                               ColumnDefinition("MyCountry")))

      val metaData = "United Kingdom,United Kingdom"

      validate(new StringReader(metaData), schema) must beLike { case Success(_) => ok }
    }

    "succeed for 2 'is' rule" in {
      val schema = Schema(List(TotalColumns(1), NoHeader()),
                          List(ColumnDefinition("Country", List(IsRule(Literal(Some("UK"))), IsRule(Literal(Some("uk")))), List(IgnoreCase()))))

      val metaData = "UK"

      validate(new StringReader(metaData), schema) must beLike { case Success(_) => ok }
    }

    "fail for 'is' rule that is not matched" in {
      val schema = Schema(List(TotalColumns(1), NoHeader()),
                          List(ColumnDefinition("Country", List(IsRule(Literal(Some("France")))))))

      val metaData = "UK"

      validate(new StringReader(metaData), schema) must beLike {
        case Failure(messages) => messages.list mustEqual List("""is("France") fails for line: 1, column: Country, value: UK""")
      }
    }

    "fail for 'is' cross reference rule that is not matched" in {
      val schema = Schema(List(TotalColumns(2), NoHeader()),
        List(ColumnDefinition("Country", List(IsRule(ColumnReference("MyCountry")))),
             ColumnDefinition("MyCountry")))

      val metaData = "United,United Kingdom"

      validate(new StringReader(metaData), schema) must beLike {
        case Failure(messages) => messages.list mustEqual List("""is($MyCountry) fails for line: 1, column: Country, value: United""")
      }
    }

    "succeed for 'isNot' rule" in {
      val schema = Schema(List(TotalColumns(1), NoHeader()),
                          List(ColumnDefinition("Country", List(IsNotRule(Literal(Some("United States")))))))

      val metaData = "United Kingdom"

      validate(new StringReader(metaData), schema) must beLike { case Success(_) => ok }
    }

    "succeed for 'isNot' cross reference rule" in {
      val schema = Schema(List(TotalColumns(2), NoHeader()),
                          List(ColumnDefinition("Country", List(IsNotRule(ColumnReference("MyCountry")))),
                               ColumnDefinition("MyCountry")))

      val metaData = "United Kingdom,United States"

      validate(new StringReader(metaData), schema) must beLike { case Success(_) => ok }
    }

    "succeed for 2 'isNot' rule" in {
      val schema = Schema(List(TotalColumns(1), NoHeader()),
                          List(ColumnDefinition("Country", List(IsNotRule(Literal(Some("United States"))), IsNotRule(Literal(Some("Kingdom")))), List(IgnoreCase()))))

      val metaData = "United Kingdom"

      validate(new StringReader(metaData), schema) must beLike { case Success(_) => ok }
    }

    "fail for 'isNot' rule that is not matched" in {
      val schema = Schema(List(TotalColumns(1), NoHeader()),
                          List(ColumnDefinition("Country", List(IsNotRule(Literal(Some("United Kingdom")))))))

      val metaData = "United Kingdom"

      validate(new StringReader(metaData), schema) must beLike {
        case Failure(messages) => messages.list mustEqual List("""isNot("United Kingdom") fails for line: 1, column: Country, value: United Kingdom""")
      }
    }

    "fail for 'isNot' cross reference rule that is not matched" in {
      val schema = Schema(List(TotalColumns(2), NoHeader()),
                          List(ColumnDefinition("Country", List(IsNotRule(ColumnReference("MyCountry")))),
                               ColumnDefinition("MyCountry")))

      val metaData = "United Kingdom,United Kingdom"

      validate(new StringReader(metaData), schema) must beLike {
        case Failure(messages) => messages.list mustEqual List("""isNot($MyCountry) fails for line: 1, column: Country, value: United Kingdom""")
      }
    }

    "succeed for 'starts' rule" in {
      val schema = Schema(List(TotalColumns(1), NoHeader()),
                          List(ColumnDefinition("Country", List(StartsRule(Literal(Some("United")))))))

      val metaData = "United Kingdom"

      validate(new StringReader(metaData), schema) must beLike { case Success(_) => ok }
    }

    "succeed for 'starts' cross reference rule" in {
      val schema = Schema(List(TotalColumns(2), NoHeader()),
                          List(ColumnDefinition("Country", List(StartsRule(ColumnReference("MyCountry")))),
                               ColumnDefinition("MyCountry")))

      val metaData = "United Kingdom,United"

      validate(new StringReader(metaData), schema) must beLike { case Success(_) => ok }
    }

    "succeed for 2 'starts' rule" in {
      val schema = Schema(List(TotalColumns(1), NoHeader()),
                          List(ColumnDefinition("Country", List(StartsRule(Literal(Some("United"))), StartsRule(Literal(Some("UNITED")))), List(IgnoreCase()))))

      val metaData = "United Kingdom"

      validate(new StringReader(metaData), schema) must beLike { case Success(_) => ok }
    }

    "fail for 'starts' rule that is not matched" in {
      val schema = Schema(List(TotalColumns(1), NoHeader()),
        List(ColumnDefinition("Country", List(StartsRule(Literal(Some("united")))))))

      val metaData = "United Kingdom"

      validate(new StringReader(metaData), schema) must beLike {
        case Failure(messages) => messages.list mustEqual List("""starts("united") fails for line: 1, column: Country, value: United Kingdom""")
      }
    }

    "fail for 'starts' cross reference rule that is not matched" in {
      val schema = Schema(List(TotalColumns(2), NoHeader()),
                          List(ColumnDefinition("Country", List(StartsRule(ColumnReference("MyCountry")))),
                               ColumnDefinition("MyCountry")))

      val metaData = "United,United Kingdom"

      validate(new StringReader(metaData), schema) must beLike {
        case Failure(messages) => messages.list mustEqual List("""starts($MyCountry) fails for line: 1, column: Country, value: United""")
      }
    }

    "succeed for 'ends' rule" in {
      val schema = Schema(List(TotalColumns(1), NoHeader()),
                          List(ColumnDefinition("Country", List(EndsRule(Literal(Some("Kingdom")))))))

      val metaData = "United Kingdom"

      validate(new StringReader(metaData), schema) must beLike { case Success(_) => ok }
    }

    "succeed for 'ends' cross reference rule" in {
      val schema = Schema(List(TotalColumns(2), NoHeader()),
                          List(ColumnDefinition("Country", List(EndsRule(ColumnReference("MyCountry")))),
                               ColumnDefinition("MyCountry")))

      val metaData = "United Kingdom,Kingdom"

      validate(new StringReader(metaData), schema) must beLike { case Success(_) => ok }
    }

    "succeed for 2 'ends' rule" in {
      val schema = Schema(List(TotalColumns(1), NoHeader()),
                          List(ColumnDefinition("Country", List(EndsRule(Literal(Some("Kingdom"))), EndsRule(Literal(Some("KINGDOM")))), List(IgnoreCase()))))

      val metaData = "United Kingdom"

      validate(new StringReader(metaData), schema) must beLike { case Success(_) => ok }
    }

    "fail for 'ends' rule that is not matched" in {
      val schema = Schema(List(TotalColumns(1), NoHeader()),
                          List(ColumnDefinition("Country", List(EndsRule(Literal(Some("kingdom")))))))

      val metaData = "United Kingdom"

      validate(new StringReader(metaData), schema) must beLike {
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

      validate(new StringReader(metaData), schemaParser.parse(new StringReader(schema)).get) must beLike {
        case Failure(messages) => messages.list mustEqual List("""ends($MyCountry) fails for line: 1, column: Country, value: United Kingdom""")
      }
    }

    "fail for multiple duplicates for unique rule" in {

      val schema =
        """@totalColumns 1 @noHeader
           Name: unique
        """
      val metaData =
        """Jim
          |Ben
          |Jim
          |Jim
        """.stripMargin

      validate(new StringReader(metaData), schemaParser.parse(new StringReader(schema)).get) must beLike {
        case Failure(messages) => messages.list mustEqual List("unique fails for line: 3, column: Name, value: Jim (original at line: 1)","unique fails for line: 4, column: Name, value: Jim (original at line: 1)")
      }
    }
  }
}