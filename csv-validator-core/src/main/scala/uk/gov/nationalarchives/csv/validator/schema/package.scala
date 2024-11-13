/*
 * Copyright (c) 2013, The National Archives <digitalpreservation@nationalarchives.gov.uk>
 * https://www.nationalarchives.gov.uk
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package uk.gov.nationalarchives.csv.validator

package object schema {
  val Uuid4Regex = "[a-f0-9]{8}-[a-f0-9]{4}-4[a-f0-9]{3}-[89ab][a-f0-9]{3}-[a-f0-9]{12}"
  //val UriRegex = """([A-Za-z0-9]+:\/\/)?([a-zA-Z0-9]+(\.[a-zA-Z0-9]+)*)?(\/|(\/([A-Za-z0-9\:@!\$&'\(\}\*\+\-_,;=~\.]+|(%[A-F0-9]{2})+))*)(\?[A-Za-z0-9]+=[A-Za-z0-9]+(&[A-Za-z0-9]+=[A-Za-z0-9]+)*)?"""

  val XsdDateTimeRegex = xsdDateNoTzComponentRegex + "T" + XsdTimeOptionalTimeZoneRegex
  val XsdDateTimeWithTimeZoneRegex = xsdDateNoTzComponentRegex + "T" + XsdTimeWithTimeZoneRegex
  val XsdDateRegex = xsdDateNoTzComponentRegex + xsdOptionalTzComponentRegex
  lazy val XsdTimeOptionalTimeZoneRegex = xsdTimeNoTzComponentRegex + xsdOptionalTzComponentRegex
  lazy val XsdTimeWithTimeZoneRegex = xsdTimeNoTzComponentRegex + xsdTzComponentRegex

  private lazy val xsdDateNoTzComponentRegex = """-?[0-9]{4}-(((0(1|3|5|7|8)|1(0|2))-(0[1-9]|(1|2)[0-9]|3[0-1]))|((0(4|6|9)|11)-(0[1-9]|(1|2)[0-9]|30))|(02-(0[1-9]|(1|2)[0-9])))"""
  private lazy val xsdTimeNoTzComponentRegex = """([0-1][0-9]|2[0-4]):(0[0-9]|[1-5][0-9]):(0[0-9]|[1-5][0-9])(\.[0-9]{3})?"""
  private lazy val xsdOptionalTzComponentRegex = """((\+|-)([0-1][0-9]|2[0-4]):(0[0-9]|[1-5][0-9])|Z)?"""
  private lazy val xsdTzComponentRegex = """((\+|-)([0-1][0-9]|2[0-4]):(0[0-9]|[1-5][0-9])|Z)"""

  val UkDateRegex = """(((0[1-9]|(1|2)[0-9]|3[0-1])\/(0(1|3|5|7|8)|1(0|2)))|((0[1-9]|(1|2)[0-9]|30)\/(0(4|6|9)|11))|((0[1-9]|(1|2)[0-9])\/02))\/[0-9]{4}"""
  val PositiveIntegerRegex = "[0-9]+"
  val UpperCaseRegex = "^[\\p{Lu}\\p{N}\\p{P}\\s]*$"
  val LowerCaseRegex = "^[\\p{Ll}\\p{N}\\p{P}\\s]*$"
  val UkDateFormat = "dd/MM/yyyy"
  val PartUkDateRegex = """(([0\?][1-9\?])|([1-2\?][0-9\?])|([3\?][0-1\?])|\*)\/(January|February|March|April|May|June|July|August|September|October|November|December|\?|\*)\/([0-9\?]{4}|\*)"""


  type FilePathBase = String
  type FileName = String
}
