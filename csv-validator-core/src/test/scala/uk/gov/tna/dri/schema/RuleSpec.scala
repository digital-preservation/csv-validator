package uk.gov.tna.dri.schema

import org.specs2.mutable.Specification
import scalaz.{Failure, NonEmptyList, Success}


/**
 * User: Jim Collins
 * Date: 2/5/13
 */
class RuleSpec extends Specification{
  val litInRule = InRule(LiteralTypeProvider("hello world"))

  val colInRule = InRule(ColumnTypeProvider("$column2"))


  "InRule with a string literal behaviour" should  {
    "succeed if inRule is embedded in value" in {
      litInRule.execute(1, Map("column1" -> "myhello world today"), ColumnDefinition("column1"), "myhello world today") must be_==(Success(true))
    }

    "succeed if inRule is at begining of value" in {
      litInRule.execute(1, Map("column1" -> "myhello world today"), ColumnDefinition("column1"), "hello world today") must be_==(Success(true))
    }

    "succeed if inRule is at end of value" in {
      litInRule.execute(1, Map("column1" -> "myhello world today"), ColumnDefinition("column1"), "my hello world") must be_==(Success(true))
    }

    "succeed if inRule is the same as value" in {
      litInRule.execute(1, Map("column1" -> "myhello world today"), ColumnDefinition("column1"), "hello world") must be_==(Success(true))
    }

    "fail if inRule is not in value" in {
      litInRule.execute(1, Map("column1" -> "myhello world today"), ColumnDefinition("column1"), "hell world today") must beLike {
        case Failure(msgs) => msgs.head mustEqual "inRule: hello world fails for line 1, column: column1, value: hell world today"
      }
    }

   }

  "InRule with a column ref behaviour" should {
    "succeed if value is in referenced column" in {
      colInRule.execute(1, Map("column1" -> "hello", "column2" -> "world"), ColumnDefinition("column1"), "my world today") must be_==(Success(true))
    }

    "fail if the value in the referenced column is not in this value" in {
      colInRule.execute(1, Map("column1" -> "hello", "column2" -> "world"), ColumnDefinition("column1"), "myhello today") must beLike {
        case Failure(msgs) => msgs.head mustEqual "inRule: world fails for line 1, column: column1, value: myhello today"
      }
    }

    "fail if the referenced column does not exist" in {
      colInRule.execute(1, Map("column1" -> "hello", "column12" -> "world"), ColumnDefinition("column1"), "myhello today") must beLike {
        case Failure(msgs) => msgs.head mustEqual "inRule: Invalid Column Name fails for line 1, column: column1, value: myhello today"
      }
    }

  }

}
