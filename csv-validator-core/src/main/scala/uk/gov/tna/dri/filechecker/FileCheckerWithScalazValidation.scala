package uk.gov.tna.dri.filechecker

import java.io.File

import scalaz._
import Scalaz._


object FileCheckerWithScalazValidation {

  def check(csvLine: String): ValidationNEL[String, DataFile] = {
    val fieldValues = csvLine.split(',') map (_.trim)
    val filePath = checkFilePath(fieldValues(0))
    val fileType = checkFileType(fieldValues(1))

    (filePath |@| fileType) { DataFile(_, _) }
  }

  private def checkFilePath(filePath: String): ValidationNEL[String, String] = {
    if (readable(filePath))
      filePath.successNel[String]
    else
      ("Could not read: " + filePath).failNel[String]
  }

  private def readable(filePath: String) = new File(filePath).canRead

  private def checkFileType(fileType: String): ValidationNEL[String, String] = {
    if (fileType == "text")
      fileType.successNel[String]
    else
      ("Invalid file type: " + fileType).failNel[String]
  }
}

case class DataFile(filePath: String, fileType: String)



