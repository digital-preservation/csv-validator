/*
 * Copyright (c) 2013, The National Archives digitalpreservation@nationalarchives.gov.uk
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package uk.gov.tna.dri

package object schema {
  val Uuid4Regex = "[a-f0-9]{8}-[a-f0-9]{4}-4[a-f0-9]{3}-[89ab][a-f0-9]{3}-[a-f0-9]{12}"
  val UriRegex = """([A-Za-z0-9]+:\/\/)?([a-zA-Z0-9]+(\.[a-zA-Z0-9]+)*)?(\/|(\/([A-Za-z0-9\:@!\$&'\(\}\*\+\-_,;=~\.]+|(%[A-F0-9]{2})+))*)(\?[A-Za-z0-9]+=[A-Za-z0-9]+(&[A-Za-z0-9]+=[A-Za-z0-9]+)*)?"""
  val XsdDateTimeRegex =  "-?[0-9]{4}-(((0(1|3|5|7|8)|1(0|2))-(0[1-9]|(1|2)[0-9]|3[0-1]))|((0(4|6|9)|11)-(0[1-9]|(1|2)[0-9]|30))|(02-(0[1-9]|(1|2)[0-9])))T([0-1][0-9]|2[0-4]):(0[0-9]|[1-5][0-9]):(0[0-9]|[1-5][0-9])(\\.[0-999])?((\\+|-)([0-1][0-9]|2[0-4]):(0[0-9]|[1-5][0-9])|Z)?"
  val XsdDateRegex =      "-?[0-9]{4}-(((0(1|3|5|7|8)|1(0|2))-(0[1-9]|(1|2)[0-9]|3[0-1]))|((0(4|6|9)|11)-(0[1-9]|(1|2)[0-9]|30))|(02-(0[1-9]|(1|2)[0-9])))((\\+|-)([0-1][0-9]|2[0-4]):(0[0-9]|[1-5][0-9])|Z)?"
  val XsdTimeRegex = "([0-1][0-9]|2[0-4]):(0[0-9]|[1-5][0-9]):(0[0-9]|[1-5][0-9])(\\.[0-999])?((\\+|-)([0-1][0-9]|2[0-4]):(0[0-9]|[1-5][0-9])|Z)?"

  val UkDateRegex = "(((0[1-9]|(1|2)[0-9]|3[0-1])\\/(0(1|3|5|7|8)|1(0|2)))|((0[1-9]|(1|2)[0-9]|30)\\/(0(4|6|9)|11))|((0[1-9]|(1|2)[0-9])\\/02))\\/[0-9]{4}"
  val PositiveIntegerRegex = "[0-9]+"
  val UkDateFormat = "dd/MM/YYYY"
  val PartUkDateRegex = """(([0\?][1-9\?])|([1-2\?][0-9\?])|([3\?][0-1\?])|\*)\/(January|February|March|April|May|June|July|August|September|October|November|December|\?|\*)\/([0-9\?]{4}|\*)"""
}
