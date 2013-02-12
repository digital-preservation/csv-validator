package uk.gov.tna.dri.schema

import scalaz._
import Scalaz._
import uk.gov.tna.dri.metadata.Row

trait Rule {
  def execute(columnIndex: Int, row: Row, schema: Schema): ValidationNEL[String, Any]
}