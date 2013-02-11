package uk.gov.tna.dri.validator

import java.io.{FileReader, File}
import uk.gov.tna.dri.schema.{Schema, SchemaParser}
import scalaz.{Success => SuccessZ, Failure => FailureZ, _}
import Scalaz._

object MetaDataValidatorApp extends App with MetaDataValidator with SchemaParser {

  checkArguments(args.toList) match {

    case FailureZ(errors) => println(prettyPrint(errors))

    case SuccessZ(_) => {
      val (metaDataFile, schemaFile) = inputFilePaths(args.toList)
      println("Validating...")

      validate(metaDataFile, schemaFile, false) match {
        case FailureZ(errors) => println(prettyPrint(errors))
        case SuccessZ(_) => println("PASS")
      }
    }
  }

  def checkArguments(args: List[String]): ValidationNEL[String, List[String]] = {
    checkArgumentCount(args) match {
      case SuccessZ(args) => checkFilesReadable(args.take(2))
      case fail => fail
    }
  }

  def validate(metaDataFile: String, schemaFile: String, fastFail: Boolean = false): MetaDataValidation[Any] = {
    parseSchema(schemaFile) match {
      case FailureZ(errors) => errors.fail[Any]
      case SuccessZ(schema) => validate(new FileReader(metaDataFile), schema, fastFail)
    }
  }

  private def parseSchema(schemaFilePath: String): ValidationNEL[String, Schema] = {
    parse(new FileReader(schemaFilePath)) match {
      case Success(schema: Schema, _) => schema.successNel[String]
      case NoSuccess(message, next) => s"Schema Parse Error: ${message} at line ${next.pos.line}, column ${next.pos.column}".failNel[Schema]
    }
  }

  private def checkArgumentCount(args: List[String]) = {
    if (!argumentCountValid(args)) usage.failNel[List[String]]
    else args.successNel[String]
  }

  private def argumentCountValid(args: List[String]) = {
    args match  {
      case _ :: _ :: "--failFast" :: Nil => true
      case _ :: _ :: Nil => true
      case _ => false
    }

  }

  private def usage = "Usage: validate <meta-data file path> <schema file path> [--failFast]"

  private def inputFilePaths(args: List[String]) = (args(0), args(1))

  private def checkFilesReadable(args: List[String]) = args.map(fileReadable).sequence[MetaDataValidation, String]

  private def fileReadable(filePath: String): ValidationNEL[String, String] = if (new File(filePath).canRead) filePath.successNel[String] else fileNotReadableMessage(filePath).failNel[String]

  private def fileNotReadableMessage(filePath: String) = "Unable to read file : " + filePath

  private def prettyPrint(l: NonEmptyList[String]) = l.list.mkString(eol)
}
