package uk.gov.tna.dri.schema

import org.specs2.mutable.Specification

class DuplicateColumnsSpec extends Specification {

  "column definitions" should {

    "report with position in columns" in {
      val parser = new SchemaParser {}
      val columns = ColumnDefinition("firstname") :: ColumnDefinition("surname") :: ColumnDefinition("firstname") :: Nil
      val columnsWithDuplicateIndexes =  parser.duplicateColumns( columns )

      columnsWithDuplicateIndexes mustEqual Map( (ColumnDefinition("firstname") -> List( 0, 2)) )
    }

    "find no duplicates in a unique collection of columns" in {
      val parser = new SchemaParser {}
      val columns = ColumnDefinition("firstname") :: ColumnDefinition("surname") :: Nil
      val columnsWithDuplicateIndexes =  parser.duplicateColumns( columns )

      columnsWithDuplicateIndexes mustEqual Map.empty[ColumnDefinition,List[Int]]
    }


  }
}
