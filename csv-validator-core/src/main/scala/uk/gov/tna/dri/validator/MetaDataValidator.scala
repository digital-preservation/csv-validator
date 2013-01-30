package uk.gov.tna.dri.validator

import uk.gov.tna.dri.schema.{RegexRule, Schema}
import au.com.bytecode.opencsv.CSVReader
import java.io.Reader
import scala.collection.JavaConversions._
import scalaz._
import Scalaz._
import util.matching.Regex

trait MetaDataValidator {

  def validate(csv: Reader, schema: Schema) = {
    val rows = new CSVReader(csv).readAll() toList

    val totalCols = totalColumns(rows, schema)
    val reg = true.successNel[String]

    (totalCols |@| reg) tupled
  }

  def totalColumns(rows: List[Array[String]], schema: Schema) = {
    rows.zipWithIndex.find(r => r._1.length != schema.totalColumns) match {
      case Some((row, rowIndex)) => s"Expected @TotalColumns of ${schema.totalColumns} and found ${row.length} on line ${rowIndex + 1}".failNel[Boolean]
      case _ => true.successNel[String]
    }
  }


  def regexForValue(value: String, rule: RegexRule) : Validation[String, Boolean] = {
    val regex = rule.regex.pattern.pattern
    if (value matches regex) true.success[String] else s"Value: ${value} does not match Regex: ${regex}".fail[Boolean]
  }

  def regexForRow(row: List[String], rule: RegexRule) = {
    row match {
      case first :: t => regexForValue(first, rule)
      case _ => "Column vale missing".fail[Boolean]
    }
  }

  def regex(rows: List[List[String]], rule: RegexRule) = {
    val validations = rows map (row => regexForRow(row, rule) liftFailNel)
    validations.sequence[({type x[a] = ValidationNEL[String, a]})#x, Boolean]
  }
}
