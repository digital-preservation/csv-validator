package uk.gov.tna.dri.schema

import org.specs2.mutable.Specification
import uk.gov.tna.dri.metadata.{Cell, Row}
import scalaz.{Failure, Success}

/**
 * User: Jim Collins
 * Date: 2/13/13
 */
class NotRuleSpec extends Specification{
  val notRule = NotRule(LiteralTypeProvider("myhello world today"))

  "NorRule with a string literal behaviour" should  {
    "succeed if NotRule is not equal to value" in {
      notRule.execute(0, Row(List(Cell("hello world today")), 1), Schema(1, List(ColumnDefinition("column1")))) must be_==(Success(true))
    }

    "fail if the value equals the rule" in {
      val litRule = NotRule(LiteralTypeProvider("my hello world"))
      litRule.execute(0, Row(List(Cell("my hello world")), 1), Schema(1, List(ColumnDefinition("column1")))) must beLike {
        case Failure(msgs) => msgs.head mustEqual "in: my hello world fails for line 1, column: column1, value: my hello world"
      }
    }
  }
}
