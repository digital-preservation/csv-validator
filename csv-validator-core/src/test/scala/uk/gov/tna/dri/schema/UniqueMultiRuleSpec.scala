package uk.gov.tna.dri.schema

import org.specs2.mutable.Specification
import uk.gov.tna.dri.metadata.{Cell, Row}
import scalaz.{Failure, Success}

class UniqueMultiRuleSpec extends Specification {

  "unique multi rule" should {

    "succeed if all column values are distinct" in {
      val schema = Schema(List(TotalColumns(1)), List(ColumnDefinition("Name"), ColumnDefinition("Legs")))
      val rule = UniqueMultiRule( "Legs" :: Nil )

      rule.evaluate(0, Row(Cell("r2d2") :: Cell("3") :: Nil, 1), schema)
      rule.evaluate(0, Row(Cell("c3po") :: Cell("2") :: Nil, 2), schema) must beLike { case Success(_) => ok }
    }

    "succeed if duplicate column but 2nd is distinct" in {
      val schema = Schema(List(TotalColumns(1)), List(ColumnDefinition("Name"), ColumnDefinition("Legs")))
      val rule = UniqueMultiRule( "Legs" :: Nil )

      rule.evaluate(0, Row(Cell("r2d2") :: Cell("3") :: Nil, 1), schema)
      rule.evaluate(0, Row(Cell("r2d2") :: Cell("2") :: Nil, 2), schema) must beLike { case Success(_) => ok }
    }

    "fail if there are duplicate on all columns column values" in {
      val schema = Schema(List(TotalColumns(1)), List(ColumnDefinition("Name"), ColumnDefinition("Legs"), ColumnDefinition("Color")))
      val rule = UniqueMultiRule( "Legs" :: "Color" :: Nil )

      rule.evaluate(0, Row(Cell("r2d2") :: Cell("3") :: Cell("blue") :: Nil, 1), schema)
      rule.evaluate(0, Row(Cell("r2d2") :: Cell("3") :: Cell("blue") :: Nil, 2), schema) must beLike {
        case Failure(msgs) => msgs.list mustEqual(List("unique( $Legs, $Color ) fails for line: 2, column: Name, value: \"r2d2\" (original at line: 1)"))
      }
    }

  }
}
