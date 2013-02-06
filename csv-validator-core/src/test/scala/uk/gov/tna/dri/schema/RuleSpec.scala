package uk.gov.tna.dri.schema

import org.specs2.mutable.Specification
import scalaz.{Failure, NonEmptyList, Success}


/**
 * User: Jim Collins
 * Date: 2/5/13
 */
class RuleSpec extends Specification{
  val inRule = InRule("hello world")


  "InRule behaviour" should  {
    "succeed if inRule is embedded in value" in {
      inRule.execute(1, ColumnDefinition("column1"), "myhello world today") must be_==(Success(true))
    }

    "succeed if inRule is at begining of value" in {
      inRule.execute(1, ColumnDefinition("column1"), "hello world today") must be_==(Success(true))
    }

    "succeed if inRule is at end of value" in {
      inRule.execute(1, ColumnDefinition("column1"), "my hello world") must be_==(Success(true))
    }

    "succeed if inRule is the same as value" in {
      inRule.execute(1, ColumnDefinition("column1"), "hello world") must be_==(Success(true))
    }

    "fail if inRule is not in value" in {
      inRule.execute(1, ColumnDefinition("column1"), "hell world today") must beLike {
        case Failure(msgs) => msgs.head mustEqual "inRule: hello world fails for line 1, column: column1, value: hell world today"
      }
    }

   }

}
