package uk.gov.tna.dri.validator

import java.io.{FileReader, File}
import uk.gov.tna.dri.schema.{Schema, SchemaParser}

object MetaDataValidatorApp extends App with MetaDataValidator with SchemaParser {

  assert(argumentCountValid(args), usage)

  val (metaDataFilePath, schemaFilePath) = inputFilePaths(args)
  assert(fileReadable(metaDataFilePath), fileNotReadableMessage(metaDataFilePath))
  assert(fileReadable(schemaFilePath), fileNotReadableMessage(schemaFilePath))

  println("Checking Schema...")

  parse(new FileReader(schemaFilePath)) match {
    case Success(schema: Schema, _) => {
      println("Validating Meta-Data File...")
      println("Result : " + validate(new FileReader(metaDataFilePath), schema))
    }

    case NoSuccess(message, next) =>
      println(s"${message} at line ${next.pos.line}, column ${next.pos.column}")
      System.exit(1)
  }

  def argumentCountValid(args: Array[String]) = args.length == 2

  def usage = "Usage: validate <meta-data file path> <schema file path>"

  def inputFilePaths(args: Array[String]) = (args(0), args(1))

  def fileReadable(filePath: String) = new File(filePath).canRead

  def fileNotReadableMessage(filePath: String) = "Unable to read file : " + filePath

  private def assert(assertion: Boolean, message: String) = if (!assertion) {
    println(message)
    System.exit(1)
  }
}
