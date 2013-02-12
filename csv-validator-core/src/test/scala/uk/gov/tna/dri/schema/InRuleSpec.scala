package uk.gov.tna.dri.schema

import org.specs2.mutable.Specification
import scalaz.{Failure, Success}
import uk.gov.tna.dri.metadata.{Cell, Row}

class InRuleSpec extends Specification{
  val inRule = InRule(LiteralTypeProvider("myhello world today"))

  "InRule with a string literal behaviour" should  {
    "succeed if inRule is embedded in value" in {
      inRule.execute(0, Row(List(Cell("hello world")), 1), Schema(1, List(ColumnDefinition("column1")))) must be_==(Success(true))
    }

    "succeed if inRule is at begining of value" in {
      inRule.execute(0, Row(List(Cell("hello world today")), 1), Schema(1, List(ColumnDefinition("column1")))) must be_==(Success(true))
    }

    "succeed if inRule is at end of value" in {
      val litRule = InRule(LiteralTypeProvider("my hello world"))
      litRule.execute(0, Row(List(Cell("hello world")), 1), Schema(1, List(ColumnDefinition("column1")))) must be_==(Success(true))
    }

    "succeed if inRule is the same as value" in {
      inRule.execute(0, Row(List(Cell("hello world")), 1), Schema(1, List(ColumnDefinition("column1")))) must be_==(Success(true))
    }

    "fail if inRule is not in value" in {
      val litRule = InRule(LiteralTypeProvider("my hello world"))
      litRule.execute(0, Row(List(Cell("hell world today")), 1), Schema(1, List(ColumnDefinition("column1")))) must beLike {
        case Failure(msgs) => msgs.head mustEqual "in: my hello world fails for line 1, column: column1, value: hell world today"
      }
    }
   }
}