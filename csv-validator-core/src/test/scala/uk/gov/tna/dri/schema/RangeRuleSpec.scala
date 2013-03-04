package uk.gov.tna.dri.schema

import org.specs2.mutable.Specification
import uk.gov.tna.dri.metadata.{Cell, Row}
import scalaz.{Success, Failure}


class RangeRuleSpec extends Specification {

  "RangeRule" should  {
    val globalDirectives = List(TotalColumns(1))
    val schema = Schema(globalDirectives, List(ColumnDefinition("Country")))

    "fail when non numeric number passed" in {
      val rangeRule = new RangeRule(1,2)

      rangeRule.evaluate(0, Row(List(Cell("Germany")), 1), schema) must beLike {
        case Failure(messages) => messages.list mustEqual List("""range fails for line: 1, column: Country, value: Germany""")
      }
    }

    "pass when we test integer boundaries" in {
      val rangeRule = new RangeRule(Int.MinValue,(Int.MaxValue))

      rangeRule.evaluate(0, Row(List(Cell((Int.MaxValue).toString)), 1), schema)  mustEqual Success(true)
    }

    "fail when we test small decimal outside range" in {
      val rangeRule = new RangeRule(0.01,0.1)

      rangeRule.evaluate(0, Row(List(Cell(("0.00999999999999999999999999999999"))), 1), schema)  must beLike {
        case Failure(messages) => messages.list mustEqual List("""range fails for line: 1, column: Country, value: 0.00999999999999999999999999999999""")
      }
    }
  }
}
