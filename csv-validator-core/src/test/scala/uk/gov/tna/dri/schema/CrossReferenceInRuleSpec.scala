package uk.gov.tna.dri.schema

import org.specs2.mutable.Specification
import scalaz.{Failure, Success}
import uk.gov.tna.dri.metadata.{Cell, Row}

class CrossReferenceInRuleSpec extends Specification{

  "InRule with a column ref behaviour" should {

    "succeed if value is in referenced column" in {
      val referencedColumn = ColumnDefinitionX("Col2")
      val rule = CrossReferenceInRule(referencedColumn)
      val columnDefs = ColumnDefinitionX("Name", List(rule)) :: referencedColumn :: Nil

      val schema = SchemaX(columnDefs)

      rule.execute(1, Row(List(Cell("this"), Cell("this is in here")), 1), schema) must beLike {
        case Success(_) => ok
      }
    }

    "fail if value is not in referenced column" in {
      val referencedColumn = ColumnDefinitionX("Col2")
      val rule = CrossReferenceInRule(referencedColumn)
      val columnDefs = ColumnDefinitionX("Name", List(rule)) :: referencedColumn :: Nil
      val schema = SchemaX(columnDefs)

      rule.execute(1, Row(List(Cell("this"), Cell("that is in here")), 1), schema) must beLike {
        case Failure(msg) => msg.head mustEqual "fail"
      }
    }
  }

}