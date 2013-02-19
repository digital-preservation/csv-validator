package uk.gov.tna.dri.schema

import org.specs2.mutable.Specification
import scalaz.{Failure, Success}
import uk.gov.tna.dri.metadata.{Cell, Row}

class InRuleSpec extends Specification {

  "InRule with a string literal behaviour" should  {
    val globalDirsOne = List(TotalColumns(1))

    "succeed if inRule is embedded in value" in {
      val inRule = InRule(Literal(Some("myhello world today")))
      inRule.evaluate(0, Row(List(Cell("hello world")), 1), Schema(globalDirsOne, List(ColumnDefinition("column1")))) mustEqual Success(true)
    }

    "succeed if inRule is the same as value" in {
      val inRule = InRule(Literal(Some("hello world")))
      inRule.evaluate(0, Row(List(Cell("hello world")), 1), Schema(globalDirsOne, List(ColumnDefinition("column1")))) mustEqual Success(true)
    }

    "fail if inRule is not in value" in {
      val inRule = InRule(Literal(Some("hello world")))

      inRule.evaluate(0, Row(List(Cell("hello world today")), 1), Schema(globalDirsOne, List(ColumnDefinition("column1")))) must beLike {
        case Failure(messages) => messages.head mustEqual "in: hello world fails for line: 1, column: column1, value: hello world today"
      }
    }

    "succeed with @IgnoreCase" in {
      val inRule = InRule(Literal(Some("hello world")))
      inRule.evaluate(0, Row(List(Cell("hello WORLD")), 1), Schema(globalDirsOne, List(ColumnDefinition("column1", Nil, List(IgnoreCase()))))) mustEqual Success(true)
    }
  }
}