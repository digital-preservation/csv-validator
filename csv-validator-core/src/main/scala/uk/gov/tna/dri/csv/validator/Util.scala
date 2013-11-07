package uk.gov.tna.dri.csv.validator

import scalax.file.Path
import scalaz._
import Scalaz._


object Util {

  type AppValidation[S] = ValidationNel[FailMessage, S]

  def checkFilesReadable(files: List[Path]) = files.map(fileReadable).sequence[AppValidation, FailMessage]

  def fileReadable(file: Path): AppValidation[FailMessage] = if (file.exists && file.canRead) SchemaMessage(file.path).successNel[FailMessage] else fileNotReadableMessage(file).failNel[FailMessage]

  def fileNotReadableMessage(file: Path) = SchemaMessage("Unable to read file : " + file.path)
}
