package uk.gov.tna.dri.validator

import java.io.{FileReader, File}
import uk.gov.tna.dri.schema.SchemaParser

object MetaDataValidatorApp extends App {

  assert(argumentCountValid(args), usage)

  val (metaDataFilePath, schemaFilePath) = inputFilePaths(args)
  assert(fileReadable(metaDataFilePath), fileNotReadableMessage(metaDataFilePath))
  assert(fileReadable(schemaFilePath), fileNotReadableMessage(schemaFilePath))

  val validator = new MetaDataValidator with SchemaParser

  println("Validating...")
  println("Result : " + validator.validate(new FileReader(metaDataFilePath), new FileReader(schemaFilePath)))

  def argumentCountValid(args: Array[String]) = args.length == 2

  def usage = "Usage: validate <meta-data file path> <schema file path>"

  def inputFilePaths(args: Array[String]) = (args(0), args(1))

  def fileReadable(filePath: String) = new File(filePath).canRead

  def fileNotReadableMessage(filePath: String) = "Unable to read file : " + filePath

  private def assert(assertion: Boolean, message: => String) = {
    if (!assertion) {
      println(message)
      System.exit(1)
    }
  }
}
