package uk.gov.tna.dri.schema

import org.specs2.mutable.Specification
import uk.gov.tna.dri.metadata.{Cell, Row}
import scalaz.{Failure, Success}

class OrRuleSpec extends Specification {

  "OrRule" should {
    "succeed when left rule validates" in {
      val globalDirectives = GlobalDirectives(TotalColumnsDirective(1))
      val schema = Schema(globalDirectives, List(ColumnDefinition("Country")))

      val leftInRule = InRule(Literal(Some("Germany")))
      val rightInRule = InRule(Literal(Some("France")))

      val orRule = OrRule(leftInRule, rightInRule)

      orRule.evaluate(0, Row(List(Cell("Germany")), 1), schema) mustEqual Success(true)
    }

    "succeed when right rule validates" in {
      val globalDirectives = GlobalDirectives(TotalColumnsDirective(1))
      val schema = Schema(globalDirectives, List(ColumnDefinition("Country")))

      val leftInRule = InRule(Literal(Some("Germany")))
      val rightInRule = InRule(Literal(Some("France")))

      val orRule = OrRule(leftInRule, rightInRule)

      orRule.evaluate(0, Row(List(Cell("France")), 1), schema) mustEqual Success(true)
    }

    "fail when left/right rules are invalid" in {
      val globalDirectives = GlobalDirectives(TotalColumnsDirective(1))
      val schema = Schema(globalDirectives, List(ColumnDefinition("Country")))

      val leftInRule = InRule(Literal(Some("Germany")))
      val rightInRule = InRule(Literal(Some("France")))

      val orRule = OrRule(leftInRule, rightInRule)

      orRule.evaluate(0, Row(List(Cell("UK")), 1), schema) must beLike {
        case Failure(messages) => messages.list mustEqual List("or: in: Germany in: France: fails for line: 1, column: Country, value: UK")
      }
    }

    "fail when left cross reference rule is invalid and right rule is invalid" in {
      val globalDirectives = GlobalDirectives(TotalColumnsDirective(1))
      val schema = Schema(globalDirectives, List(ColumnDefinition("Country")))

      val leftInRule = InRule(ColumnReference("ConfigurableCountry"))
      val rightInRule = InRule(Literal(Some("France")))

      val orRule = OrRule(leftInRule, rightInRule)

      orRule.evaluate(0, Row(List(Cell("UK")), 1), schema) must throwA[IndexOutOfBoundsException]
    }
  }
}