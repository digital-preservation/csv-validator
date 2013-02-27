package uk.gov.tna.dri.validator

import java.io.{FileReader, File}
import uk.gov.tna.dri.schema.{Schema, SchemaParser}
import scalaz.{Success => SuccessZ, Failure => FailureZ, _}
import Scalaz._
import scala.App

object MetaDataValidatorCommandLineApp extends App {
  type AppValidation[S] = ValidationNEL[String, S]

  val exitCode = run(args)
  System.exit(exitCode)

  def run(args: Array[String]): Int = {

    val (failFast, fileArgs) = failFastAndFileArgs(args.toList)

    checkFileArguments(fileArgs) match {

      case FailureZ(errors) => println(prettyPrint(errors)); return 1

      case SuccessZ(_) => {
        val (metaDataFile, schemaFile) = inputFilePaths(fileArgs)
        println("Validating...")

        val validator = if (failFast) new MetaDataValidatorApp with FailFastMetaDataValidator else new MetaDataValidatorApp with AllErrorsMetaDataValidator

        validator.parseSchema(schemaFile) match {
          case FailureZ(errors) => println(prettyPrint(errors)); return 2
          case SuccessZ(schema) => validator.validate(metaDataFile, schema) match {
            case FailureZ(errors) => println(prettyPrint(errors)); return 3
            case SuccessZ(_) => println("PASS")
          }
        }
      }
    }
    return 0
  }

  def checkFileArguments(fileArgs: List[String]): AppValidation[List[String]] = {
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

  private def fileReadable(filePath: String): AppValidation[String] = if (new File(filePath).canRead) filePath.successNel[String] else fileNotReadableMessage(filePath).failNel[String]

  private def fileNotReadableMessage(filePath: String) = "Unable to read file : " + filePath

  private def prettyPrint(l: NonEmptyList[String]) = l.list.mkString(sys.props("line.separator"))
}

trait MetaDataValidatorApp extends SchemaParser {
  this: MetaDataValidator =>

  def validate(metaDataFile: String, schema: Schema): MetaDataValidation[Any] = {
     validate(new FileReader(metaDataFile), schema)
  }

  def parseSchema(schemaFilePath: String): ValidationNEL[String, Schema] = parseAndValidate(new FileReader(schemaFilePath))
}