package uk.gov.tna.dri.filechecker

import io.Source
import java.io.{FileReader, File}
import au.com.bytecode.opencsv.CSVReader
import scala.collection.JavaConversions._


object FileChecker {
  def check(csvFile: String) = {
    val reader = new CSVReader(new FileReader(csvFile))
    !reader.readAll.map(row => readable(row(0))).exists(_ == false)
  }

  private def readable(filePath: String) = new File(filePath).canRead
}
