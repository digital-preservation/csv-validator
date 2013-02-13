package uk.gov.tna.dri.validator

import java.io.{FileReader, File}
import uk.gov.tna.dri.schema.{Schema, SchemaParser}
import scalaz.{Success => SuccessZ, Failure => FailureZ, _}
import Scalaz._
import scala.App


object MetaDataValidatorCommandLineApp extends App with SchemaParser {

  val (failFast, fileArgs) = failFastAndFileArgs(args.toList)

  checkFileArguments(fileArgs) match {

    case FailureZ(errors) => println(prettyPrint(errors)); System.exit(1)

    case SuccessZ(_) => {
      val (metaDataFile, schemaFile) = inputFilePaths(fileArgs)
      println("Validating...")

      val validator = if (failFast) new MetaDataValidatorApp with FailFastMetaDataValidator else new MetaDataValidatorApp with AllErrorsMetaDataValidator

      validator.validate(metaDataFile, schemaFile) match {
        case FailureZ(errors) => println(prettyPrint(errors)); System.exit(2)
        case SuccessZ(_) => println("PASS")
      }
    }
  }

  def checkFileArguments(fileArgs: List[String]): ValidationNEL[String, List[String]] = {
    checkFileArgumentCount(fileArgs) match {
      case SuccessZ(fileArgs) => checkFilesReadable(fileArgs)
      case fail => fail
    }
  }

  private def checkFileArgumentCount(fileArgs: List[String]) = {
    if (!fileArgumentCountValid(fileArgs)) usage.failNel[List[String]]
    else fileArgs.successNel[String]
  }

  def failFastAndFileArgs(args: List[String]) = {
    val (flags, files) = args.partition( a => a ==  "--fail-fast" || a == "-f")
    (flags.nonEmpty, files)
  }

  private def fileArgumentCountValid(fileArgs: List[String]) = fileArgs.length == 2

  private def usage = "Usage: validate [--fail-fast] <meta-data file path> <schema file path>"

  private def inputFilePaths(fileArgs: List[String]) = (fileArgs(0), fileArgs(1))

  private def checkFilesReadable(fileArgs: List[String]) = fileArgs.map(fileReadable).sequence[AppValidation, String]

  private def fileReadable(filePath: String): ValidationNEL[String, String] = if (new File(filePath).canRead) filePath.successNel[String] else fileNotReadableMessage(filePath).failNel[String]

  private def fileNotReadableMessage(filePath: String) = "Unable to read file : " + filePath

  private def prettyPrint(l: NonEmptyList[String]) = l.list.mkString(eol)

  type AppValidation[S] = ValidationNEL[String, S]

}

trait MetaDataValidatorApp extends SchemaParser {
  this: MetaDataValidator =>

  def validate(metaDataFile: String, schemaFile: String): MetaDataValidation[Any] = {
    parseSchema(schemaFile) match {
      case FailureZ(errors) => errors.fail[Any]
      case SuccessZ(schema) => validate(new FileReader(metaDataFile), schema)
    }
  }

  private def parseSchema(schemaFilePath: String): ValidationNEL[String, Schema] = {
    parse(new FileReader(schemaFilePath)) match {
      case Success(schema: Schema, _) => schema.successNel[String]
      case NoSuccess(message, next) => s"Schema Parse Error: ${message} at line ${next.pos.line}, column ${next.pos.column}".failNel[Schema]
    }
  }
}
