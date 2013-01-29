package uk.gov.tna.dri.validator

import uk.gov.tna.dri.schema.Schema
import au.com.bytecode.opencsv.CSVReader
import java.io.Reader
import scala.collection.JavaConversions._
import scalaz._
import Scalaz._
import util.matching.Regex

trait MetaDataValidator {

  def validate(csv: Reader, schema: Schema) : ValidationNEL[String, MetaData] = {
    val rows = new CSVReader(csv).readAll() toList

    val totalCols = totalColumns(rows, schema)
    val reg = regex(rows, schema)

    (totalCols |@| reg)  { MetaData(_, _) }
  }

  def totalColumns(rows: List[Array[String]], schema: Schema): ValidationNEL[String, Int] = {
    rows.zipWithIndex.find(r => r._1.length != schema.totalColumns) match {
      case Some((row, rowIndex)) => s"Expected @TotalColumns of ${schema.totalColumns} and found ${row.length} on line ${rowIndex + 1}".failNel[Int]
      case _ => schema.totalColumns.successNel[String]
    }
  }

  def regex(rows: List[Array[String]], schema: Schema): ValidationNEL[String, String] = {
    schema.regex match {
      case Some(regex) => rows.map(regexForRow(_, regex)) exists (_.isFailure) match {
        case true => "fail".failNel[String]
        case false => "success".successNel[String]
      }
      case _ => "success".successNel[String]
  }


  }

  def regexForRow(row: Array[String], regex: Regex): ValidationNEL[String, String] = {
    row exists (!_.matches(regex.pattern.pattern)) match {
      case true => s"Expected regex ${regex}".failNel[String]
      case false => row.mkString.successNel[String]
    }
  }
}

case class MetaData(numOfColumns: Int, regexResult: String)