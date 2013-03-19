package uk.gov.tna.dri

/**
 * User: Jim Collins
 * Date: 3/19/13
 */
package object schema {
  val Uuid4Regex = "[a-f0-9]{8}-[a-f0-9]{4}-4[a-f0-9]{3}-[89ab][a-f0-9]{3}-[a-f0-9]{12}"
  val XsdDateTimeRegex = "[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}"
  val XsdDateRegex = "[0-9]{4}-[0-9]{2}-[0-9]{2}"
  val UkDateRegex = "[0-9]{1,2}/[0-9]{1,2}/[0-9]{4}"
  val XsdTimeRegex = "[0-9]{2}:[0-9]{2}:[0-9]{2}"
  val PositiveIntegerRegex = "[0-9]+"
  val UkDateFormat = "dd/MM/YYYY"
}
