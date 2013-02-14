package uk.gov.tna.dri.schema

import org.specs2.mutable.Specification
import uk.gov.tna.dri.metadata.{Cell, Row}
import scalaz.{Failure, Success}

/**
 * User: Jim Collins
 * Date: 2/13/13
 */
class IsRuleSpec extends Specification {

  val isRule = IsRule(LiteralTypeProvider("myhello world today"))

  "IsRule with a string literal behaviour" should  {
    "succeed if isRule equals value" in {
      isRule.evaluate(0, Row(List(Cell("myhello world today")), 1), Schema(1, List(ColumnDefinition("column1")))) must be_==(Success(true))
    }

    "fail if value is embedded in rule" in {
      val isRule = IsRule(LiteralTypeProvider("my hello world"))

      isRule.evaluate(0, Row(List(Cell("hello")), 1), Schema(1, List(ColumnDefinition("column1")))) must beLike {
        case Failure(msgs) => msgs.head mustEqual "is: my hello world fails for line: 1, column: column1, value: hello"
      }
    }
  }
}