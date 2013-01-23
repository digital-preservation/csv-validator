package uk.gov.tna.dri.schema

import org.specs2.mutable._
import org.specs2.matcher.ParserMatchers

class SchemaParserSpec extends Specification with ParserMatchers {
  override val parsers = SchemaParser

  import SchemaParser._

  "@TotalColumns" should {

    "fail for incorrect field name" in {
      totalColumns must failOn("@ToalColumns: 23")
    }

    "fail for field name incorrect case" in {
      totalColumns must failOn("@totalColumns: 65")
    }

    "fail for missing value" in {
      totalColumns must failOn("@TotalColumns :")
    }

    "succeed for positive integer" in {
      totalColumns must succeedOn("@TotalColumns : 5").withResult(5)
    }

    "succeed for zero" in {
      totalColumns must succeedOn("@TotalColumns : 0").withResult(0)
    }

    "fail for negative integer" in {
      totalColumns must failOn("@TotalColumns : -23")
    }

    "fail for non integer" in {
      totalColumns must failOn("@TotalColumns : 132.45")
    }

    "fail for non numeric" in {
      totalColumns must failOn("@TotalColumns : blah")
    }

  }

  "Schema" should {

    "include @TotalColumns" in {
      schema must succeedOn(
        """{@TotalColumns : 5}""")
        .withResult(Schema(5))
    }

    "allow @Quoted" in {
      schema must succeedOn(
        """{@TotalColumns : 5
            @Quoted : -}""")
        .withResult(Schema(5, Some("-")))
    }
  }
}
