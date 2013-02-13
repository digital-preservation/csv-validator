package uk.gov.tna.dri.schema

import scalaz._
import Scalaz._
import uk.gov.tna.dri.metadata.Row
import java.io.File

case class FileExistsRule(rootPath: Option[String] = None) extends Rule {

  override val name = "fileExists"

  override def evaluate(columnIndex: Int, row: Row, schema: Schema): ValidationNEL[String, Any] = {
    val filePath = row.cells(columnIndex).value

    val fileExists = rootPath match {
      case Some(root) => new File(root, filePath).exists()
      case None => new File(row.cells(columnIndex).value).exists()
    }

    if (fileExists) true.successNel else s"fileExistsRule: fails for line ${row.lineNumber}, column: ${schema.columnDefinitions(columnIndex).id}, value: ${filePath}".failNel[Any]
  }
}