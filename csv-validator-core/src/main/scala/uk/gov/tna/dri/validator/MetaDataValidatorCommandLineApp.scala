package uk.gov.tna.dri.validator

import java.io.{FileReader, File}
import uk.gov.tna.dri.schema.{Schema, SchemaParser}
import scalaz.{Success => SuccessZ, Failure => FailureZ, _}
import Scalaz._
import scala.App


object MetaDataValidatorCommandLineApp extends App with SchemaParser {

  checkArguments(args.toList) match {

    case FailureZ(errors) => println(prettyPrint(errors))

    case SuccessZ(_) => {
      val (metaDataFile, schemaFile) = inputFilePaths(args.toList)
      println("Validating...")

      val validator = if (failFast(args.toList)) new MetaDataValidatorApp with FailFastMetaDataValidator else new MetaDataValidatorApp with AllErrorsMetaDataValidator

      validator.validate(metaDataFile, schemaFile) match {
        case FailureZ(errors) => println(prettyPrint(errors))
        case SuccessZ(_) => println("PASS")
      }
    }
  }

  def checkArguments(args: List[String]): ValidationNEL[String, List[String]] = {
    val fileArgs = args.filterNot( _ == "--failFast" )
    checkFileArgumentCount(fileArgs) match {
      case SuccessZ(fileArgs) => checkFilesReadable(fileArgs)
      case fail => fail
    }
  }

  private def checkFileArgumentCount(args: List[String]) = {
    if (!fileArgumentCountValid(args)) usage.failNel[List[String]]
    else args.successNel[String]
  }

  private def fileArgumentCountValid(args: List[String]) = args.length == 2

  private def failFast( args: List[String]) = args.contains("--failFast")

  private def usage = "Usage: validate [--failFast] <meta-data file path> <schema file path>"

  private def inputFilePaths(args: List[String]) = {
    val fileArgs = args.filterNot( _ == "--failFast" )
    (fileArgs(0), fileArgs(1))
  }

  private def checkFilesReadable(args: List[String]) = args.map(fileReadable).sequence[AppValidation, String]

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
