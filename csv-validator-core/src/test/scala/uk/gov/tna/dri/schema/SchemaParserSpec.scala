package uk.gov.tna.dri.schema

import org.specs2.mutable._
import org.specs2.matcher.ParserMatchers

class SchemaParserSpec extends Specification with ParserMatchers {
  override val parsers = SchemaParser
  import SchemaParser.totalColumns

  "Total columns" should {

    "allow positive integers" in {
      totalColumns must succeedOn("@TotalColumns : 5").withResult(5)
    }

    "allow zero" in {
      totalColumns must succeedOn("@TotalColumns : 0").withResult(0)
    }

    "fail on negative integers" in {
      totalColumns must failOn("@TotalColumns : -23")
    }

    "fail on non integers" in {
      totalColumns must failOn("@TotalColumns : 132.45")
    }

    "fail on non numeric" in {
      totalColumns must failOn("@TotalColumns : blah")
    }
  }
}
