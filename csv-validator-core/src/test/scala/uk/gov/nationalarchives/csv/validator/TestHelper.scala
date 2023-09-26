/*
 * Copyright (c) 2013, The National Archives <digitalpreservation@nationalarchives.gov.uk>
 * https://www.nationalarchives.gov.uk
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package uk.gov.nationalarchives.csv.validator

object TestHelper {

  implicit class RichString(str: String) {

    /** Strip all carriage returns in order to facilitate cross-platform/encoding comparisons
      */
    def removeCR: String = str.replace("\r", "")
  }

  implicit class RichFailMessages(fm: FailMessage) {
    def removeCR: FailMessage = {
      fm.copy(
        message = fm.message.removeCR
      )
    }
  }
}
