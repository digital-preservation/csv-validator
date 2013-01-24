package uk.gov.tna.dri.csv

import java.io.{InputStreamReader, FileInputStream, File}
import uk.gov.tna.dri.schema.{SchemaParser, Schema}

object CsvValidatorApp extends App {

  def defaultEncoding = "UTF-8"

  if (!argumentCountValid(args)) { println(usage); System.exit(1) }
  val (metaDataFilePath, schemaFilePath) = inputFilePaths(args)
  if (!fileReadable(metaDataFilePath)) { println(fileNotReadableMessage(metaDataFilePath)); System.exit(1)}
  if (!fileReadable(schemaFilePath)) { println(fileNotReadableMessage(schemaFilePath)); System.exit(1)}

  val validator = new CsvValidator with SchemaParser

  println("Validating...")
  
  println("Result : " + validator.validate(new InputStreamReader(new FileInputStream(metaDataFilePath), defaultEncoding), new InputStreamReader(new FileInputStream(schemaFilePath), defaultEncoding)))


  def argumentCountValid(args: Array[String]) = args.length == 2

  def usage = "Usage: validate <meta-data file path> <schema file path>"

  def inputFilePaths(args: Array[String]) = (args(0), args(1))

  def fileReadable(filePath: String) = new File(filePath).canRead

  def fileNotReadableMessage(filePath: String) = "Unable to read file : " + filePath
}
