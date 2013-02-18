package uk.gov.tna.dri.schema

import scalaz._
import Scalaz._

trait SchemaValidator {

  type SchemaVal[A] = ValidationNEL[String, A]

  def validate(schema: Schema): SchemaVal[List[ColumnDefinition]] = {
    val dupCols = duplicateColumns( schema.columnDefinitions)
    if (dupCols.isEmpty) schema.columnDefinitions.successNel[String]
    else {
      val errors: List[SchemaVal[ColumnDefinition]] = dupCols.map{ case(colDef, pos) => s"Column: ${colDef.id} has duplicates in columns ${pos.mkString(",")}".failNel[ColumnDefinition]}.toList
      errors.sequence[SchemaVal, ColumnDefinition]
    }
  }


  def duplicateColumns( col:List[ColumnDefinition] ):Map[ColumnDefinition,List[Int]] = {
    val columnsByColumnId = col.zipWithIndex.groupBy { case (id, pos) => id }
    columnsByColumnId.filter( _._2.length > 1 ).map { case (id,idAndPos) => (id, idAndPos.map{ case (id, pos) => index2Column(pos)}) }
  }

  def index2Column(i:Int) = i+1
}
