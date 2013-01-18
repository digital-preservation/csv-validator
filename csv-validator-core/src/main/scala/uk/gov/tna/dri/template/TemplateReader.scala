package uk.gov.tna.dri.template

import java.io.File
import scala.io.Source

object TemplateReader {
  def read(path: String) = {
    val json = Source.fromFile(path).getLines().mkString
    if (json.contains("numberOfColumns")) Some(Template(10)) else None
  }

  def fileReadable(path: String) = new File(path).canRead
}
