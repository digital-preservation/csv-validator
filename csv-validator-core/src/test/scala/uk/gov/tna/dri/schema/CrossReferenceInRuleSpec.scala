package uk.gov.tna.dri.schema

import org.specs2.mutable.Specification
import scalaz.{Failure, Success}
import uk.gov.tna.dri.metadata.{Cell, Row}

class CrossReferenceInRuleSpec extends Specification {

  val nameInFullNameRule = CrossReferenceInRule("FullName")
  val columnDefinitions = ColumnDefinition("Name") :: ColumnDefinition("FullName") :: Nil

  "CrossReferenceInRule" should {

    "succeed if value is in referenced column" in {
      nameInFullNameRule.execute(0, Row(List(Cell("Scooby"), Cell("Scooby Doo")), 1), Schema(2, columnDefinitions)) must beLike {
        case Success(_) => ok
      }
    }

    "fail if value is not in reference column" in {
      nameInFullNameRule.execute(0, Row(List(Cell("Scrappy"), Cell("Scooby Doo")), 1), Schema(2, columnDefinitions)) must beLike {
        case Failure(messages) => messages.list must containTheSameElementsAs(List("in($FullName) fails for line 1, column: Name, value: Scrappy"))
      }
    }

    "fail if the referenced column does not exist" in {
      val nameInNonExistentColumnRule = CrossReferenceInRule("DoesntExist")
      nameInNonExistentColumnRule.execute(0, Row(List(Cell("Scrappy"), Cell("Scooby Doo")), 1), Schema(2, columnDefinitions)) must beLike {
        case Failure(messages) => messages.list must containTheSameElementsAs(List("in($DoesntExist) references a non-existent column"))
      }
    }
  }

}