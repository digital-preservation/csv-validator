package uk.gov.tna.dri.validator

import uk.gov.tna.dri.schema.Schema
import au.com.bytecode.opencsv.CSVReader
import java.io.Reader
import scala.collection.JavaConversions._
import scalaz._
import Scalaz._

trait MetaDataValidator {

  def validate(csv: Reader, schema: Schema) : ValidationNEL[String, Int] = {
    val rows = new CSVReader(csv).readAll()

    rows.zipWithIndex.find( r => r._1.length != schema.totalColumns ) match {
      case Some((row, rowIndex)) => s"Expected @TotalColumns of ${schema.totalColumns} and found ${row.length} on line ${rowIndex + 1}".failNel[Int]
      case _ => schema.totalColumns.successNel[String]
    }
  }
}