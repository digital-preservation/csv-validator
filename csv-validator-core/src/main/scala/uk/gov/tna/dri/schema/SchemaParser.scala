package uk.gov.tna.dri.schema

import uk.gov.tna.dri.io.Resource

/**
 * Generate Schema by parsing a Resource.
 * @author David Ainslie
 */
object SchemaParser {
  /**
   * Implicitly convert a String to an Int
   * @param s String to be converted to an Int
   * @return Int of given String
   */
  implicit def strToInt(s: String) = s.toInt

  /**
   * Parse given resource generating a Schema - exception will be thrown for missing mandatory fields.
   * @param resource
   * @return Schema generated from given Resource
   */
  def parse(resource: Resource[String]): Schema = {
    val map = resource.contents map {text =>
      val keyValue = text.split("=")
      (keyValue(0).trim -> keyValue(1).trim)
    } toMap

    Schema(map("name"), map("totalColumns"))
  }
}
