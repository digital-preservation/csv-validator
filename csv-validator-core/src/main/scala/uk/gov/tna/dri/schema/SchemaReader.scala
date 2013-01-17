package uk.gov.tna.dri.schema

import java.io.File

object SchemaReader {
  def fileReadable(path: String) = new File(path).canRead
}
