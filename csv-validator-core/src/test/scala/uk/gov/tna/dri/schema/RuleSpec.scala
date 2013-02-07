package uk.gov.tna.dri.schema

import org.specs2.mutable.Specification
import scalaz.{Failure, Success}
import uk.gov.tna.dri.metadata.{Cell, Row}

/**
 * User: Jim Collins
 * Date: 2/5/13
 */
class RuleSpec extends Specification{
  val litInRule = InRule(LiteralTypeProvider("hello world"))

  val colInRule = InRule(ColumnTypeProvider("$column2"))

  "InRule with a string literal behaviour" should  {
    "succeed if inRule is embedded in value" in {
      litInRule.execute(CellContext(0, Row(List(Cell("myhello world today")), 1), Schema(1, List(ColumnDefinition("column1")))) ) must be_==(Success(true))
    }

    "succeed if inRule is at begining of value" in {
      litInRule.execute(CellContext(0, Row(List(Cell("hello world today")), 1), Schema(1, List(ColumnDefinition("column1")))) ) must be_==(Success(true))
    }

    "succeed if inRule is at end of value" in {
      litInRule.execute(CellContext(0, Row(List(Cell("my hello world")), 1), Schema(1, List(ColumnDefinition("column1")))) ) must be_==(Success(true))
    }

    "succeed if inRule is the same as value" in {
      litInRule.execute(CellContext(0, Row(List(Cell("hello world")), 1), Schema(1, List(ColumnDefinition("column1")))) ) must be_==(Success(true))
    }

    "fail if inRule is not in value" in {
      litInRule.execute(CellContext(0, Row(List(Cell("hell world today")), 1), Schema(1, List(ColumnDefinition("column1")))) ) must beLike {
        case Failure(msgs) => msgs.head mustEqual "inRule: hello world fails for line 1, column: column1, value: hell world today"
      }
    }
   }

  "InRule with a column ref behaviour" should {
    "succeed if value is in referenced column" in {
      colInRule.execute(CellContext(0, Row(List(Cell("my world today"), Cell("world")), 1), Schema(2, List(ColumnDefinition("column1"), ColumnDefinition("column2")))) ) must be_==(Success(true))
    }

    "fail if the value in the referenced column is not in this value" in {
      colInRule.execute(CellContext(0, Row(List(Cell("myhello today"), Cell("world")), 1), Schema(2, List(ColumnDefinition("column1"), ColumnDefinition("column2")))) ) must beLike {
        case Failure(msgs) => msgs.head mustEqual "inRule: world fails for line 1, column: column1, value: myhello today"
      }
    }

    "fail if the referenced column does not exist" in {
      colInRule.execute(CellContext(0, Row(List(Cell("myhello today"), Cell("today")), 1), Schema(2, List(ColumnDefinition("column1"), ColumnDefinition("column12")))) ) must beLike {
        case Failure(msgs) => msgs.head mustEqual "inRule: Invalid Column Name fails for line 1, column: column1, value: myhello today"
      }
    }
  }
}