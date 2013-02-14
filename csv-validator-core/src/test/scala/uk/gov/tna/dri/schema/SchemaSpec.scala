package uk.gov.tna.dri.schema

import org.specs2.mutable.Specification
import uk.gov.tna.dri.metadata.{Cell, Row}
import scalaz.Success
import scala._
import uk.gov.tna.dri.schema.ColumnTypeProvider
import uk.gov.tna.dri.schema.Schema
import uk.gov.tna.dri.schema.InRule
import uk.gov.tna.dri.schema.ColumnDefinition

/**
 * User: Jim Collins
 * Date: 2/13/13
 */
class SchemaSpec extends Specification{

  "Schema creation" should  {

    "succeed for a schema with no rules" in {
      val schema = Schema(1, List(ColumnDefinition("Column1"))) must beLike{
        case Schema(1, _)  => ok
      }
    }

    "fail when totalcolumns does not match number of columndefinitions" in {
      val schema = Schema(2, List(ColumnDefinition("Column1"))) must throwA( new java.lang.IllegalArgumentException("requirement failed: totalColumns: 2 must be the same as the number of column definitions: 1"))
    }

    "succeed if Column1 correctly has InRule that points to Column2" in {
      val schema = Schema(2, List(
        ColumnDefinition("Column1", List(InRule(ColumnTypeProvider("Column2")))),
        ColumnDefinition("Column2", List())
      ))  must beLike{
        case Schema(_, _)  => ok
      }
    }

    "fail when one invalid column reference" in {
      val schema = Schema(2, List(
        ColumnDefinition("Column1", List(InRule(ColumnTypeProvider("NotAColumn")))),
        ColumnDefinition("Column2",   List())
      ))  must throwA( new java.lang.IllegalArgumentException("requirement failed: Column: Column1 has invalid cross reference in: NotAColumn"))
    }

    "fail when there are two rules and one is invalid" in {
      val schema = Schema(2, List(
        ColumnDefinition("Column1", List(InRule(ColumnTypeProvider("Column2")), InRule(ColumnTypeProvider("NotAColumn2")))),
        ColumnDefinition("Column2",   List())
      ))  must throwA( new java.lang.IllegalArgumentException("requirement failed: Column: Column1 has invalid cross reference in: NotAColumn2"))
    }

    "fail when two rules are invalid " in {
      val schema = Schema(2, List(
        ColumnDefinition("Column1", List(InRule(ColumnTypeProvider("NotAColumn1")), InRule(ColumnTypeProvider("NotAColumn2")))),
        ColumnDefinition("Column2",   List())
      ))  must throwA( new java.lang.IllegalArgumentException("requirement failed: Column: Column1 has invalid cross reference in: NotAColumn1,in: NotAColumn2"))
    }


    "fail when two columns have two rules and each has one invalid column" in {
      val schema = Schema(2, List(
        ColumnDefinition("Column1", List(InRule(ColumnTypeProvider("Column2")), InRule(ColumnTypeProvider("NotAColumn2")))),
        ColumnDefinition("Column2", List(InRule(ColumnTypeProvider("NotAColumn3")), InRule(ColumnTypeProvider("Column2"))))
      ))  must throwA( new java.lang.IllegalArgumentException("requirement failed: Column: Column1 has invalid cross reference in: NotAColumn2\nColumn: Column2 has invalid cross reference in: NotAColumn3"))
    }

    "fail when two columns have two rules and each has one invalid column with diffferent rules" in {
      val schema = Schema(5, List(
        ColumnDefinition("Column1", List(IsRule(ColumnTypeProvider("Column1")), IsRule(ColumnTypeProvider("NotAColumn1")))),
        ColumnDefinition("Column2", List(NotRule(ColumnTypeProvider("Column2")), NotRule(ColumnTypeProvider("NotAColumn2")))),
        ColumnDefinition("Column3", List(InRule(ColumnTypeProvider("Column3")), InRule(ColumnTypeProvider("NotAColumn3")))),
        ColumnDefinition("Column4", List(StartsRule(ColumnTypeProvider("Column4")), StartsRule(ColumnTypeProvider("NotAColumn4")))),
        ColumnDefinition("Column5", List(EndsRule(ColumnTypeProvider("Column5")), EndsRule(ColumnTypeProvider("NotAColumn5"))))
      ))  must throwA( new java.lang.IllegalArgumentException(
        """requirement failed: Column: Column1 has invalid cross reference is: NotAColumn1
          |Column: Column2 has invalid cross reference not: NotAColumn2
          |Column: Column3 has invalid cross reference in: NotAColumn3
          |Column: Column4 has invalid cross reference starts: NotAColumn4
          |Column: Column5 has invalid cross reference ends: NotAColumn5""".stripMargin
      ))
    }
  }
}
