package uk.gov.tna.dri.schema

import org.specs2.mutable.Specification
import uk.gov.tna.dri.metadata.{Cell, Row}
import scalaz.{Failure, Success}

/**
 * User: Jim Collins
 * Date: 2/13/13
 */
class StartsRuleSpec extends Specification {

  val startsRule = StartsRule(LiteralTypeProvider("myhello world today today"))

  "StartsRule with a string literal behaviour" should  {
    "succeed if StartsRule is starts with value" in {
      startsRule.evaluate(0, Row(List(Cell("myhello world today ")), 1), Schema(1, List(ColumnDefinition("column1")))) must be_==(Success(true))
    }

    "fail if the value does not start with" in {
      val startsRule = StartsRule(LiteralTypeProvider("my hello world"))

      startsRule.evaluate(0, Row(List(Cell("hello world")), 1), Schema(1, List(ColumnDefinition("column1")))) must beLike {
        case Failure(msgs) => msgs.head mustEqual "starts: my hello world fails for line: 1, column: column1, value: hello world"
      }
    }
  }
}