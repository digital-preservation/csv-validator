/**
 * Copyright (c) 2022, The National Archives <digitalpreservation@nationalarchives.gov.uk>
 * https://www.nationalarchives.gov.uk
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package uk.gov.nationalarchives.csv.validator
package cmd

import java.text.DecimalFormat

object CommandLineProgressCallback extends ProgressCallback {
  private val numberFormat = new DecimalFormat("0% \n")

  override def update(complete: Percentage): Unit =
    Console.out.println(numberFormat.format(complete/100))

  override def update(total: Int, processed: Int): Unit =
    Console.out.println(s"processing ${processed} of ${total}")

}
