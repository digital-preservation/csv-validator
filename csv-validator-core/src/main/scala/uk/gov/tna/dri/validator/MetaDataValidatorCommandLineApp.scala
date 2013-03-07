package uk.gov.tna.dri.validator

import java.io.{FileReader, File}
import uk.gov.tna.dri.schema.{Schema, SchemaParser}
import scalaz.{Success => SuccessZ, Failure => FailureZ, _}
import Scalaz._
import scala.App

object  SystemExits {
  val ValidCsv = 0
  val IncorrectArguments = 1
  val InvalidSchema = 2
  val InvalidCsv = 3
}
object MetaDataValidatorCommandLineApp extends App {
  type AppValidation[S] = ValidationNEL[String, S]

  val exitCode = run(args)
  System.exit(exitCode)

  def run(args: Array[String]): Int = {

    val pathSubstitutionsList: List[(String,String)] = findSubstitutionPaths(args.toList)

    val (failFast, fileArgs) = failFastAndFileArgs(args.toList)

    checkFileArguments(fileArgs) match {

      case FailureZ(errors) => println(prettyPrint(errors)); SystemExits.IncorrectArguments

      case SuccessZ(_) => {
        val (metaDataFile, schemaFile) = inputFilePaths(fileArgs)
        println("Validating...")

        val validator = if (failFast) new MetaDataValidatorApp with FailFastMetaDataValidator { val pathSubstitutions = pathSubstitutionsList } else new MetaDataValidatorApp with AllErrorsMetaDataValidator { val pathSubstitutions = pathSubstitutionsList }

        validator.parseSchema(schemaFile) match {
          case FailureZ(errors) => println(prettyPrint(errors)); SystemExits.InvalidSchema
          case SuccessZ(schema) => validator.validate(metaDataFile, schema) match {
            case FailureZ(errors) => println(prettyPrint(errors)); SystemExits.InvalidCsv
            case SuccessZ(_) => println("PASS"); SystemExits.ValidCsv
          }
        }
      }
    }
  }

  def checkFileArguments(fileArgs: List[String]): AppValidation[List[String]] = {
    checkFileArgumentCount(fileArgs) match {
      case SuccessZ(fargs) => checkFilesReadable(fargs)
      case fail => fail
    }
  }

  private def checkFileArgumentCount(fileArgs: List[String]) = {
    if (!fileArgumentCountValid(fileArgs)) usage.failNel[List[String]]
    else fileArgs.successNel[String]
  }

  def failFastAndFileArgs(args: List[String]) = {
    val (flags, files) = args.partition( a => a.startsWith("-")) // ==  "--fail-fast" || a == "-f")
    (flags.contains( "--fail-fast" ) || flags.contains( "-f" ), files)
  }

  private def fileArgumentCountValid(fileArgs: List[String]) = fileArgs.length == 2

  def findSubstitutionPaths(args: List[String]): List[(String,String)]= {
    val cmdName = "--path="
    val paths =  args.filter( a => a.startsWith(cmdName))
    paths.map( p => {
      val i = p.drop(cmdName.length).split(",").lift
      (i(0).getOrElse(""),i(1).getOrElse(""))
    })
  }

  private def usage = "Usage: validate [--fail-fast] [--path=<from>,<to>]* <meta-data file path> <schema file path>"

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