/**
 * Copyright (c) 2013, The National Archives <digitalpreservation@nationalarchives.gov.uk>
 * http://www.nationalarchives.gov.uk
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package uk.gov.nationalarchives.csv.validator

import java.io.File

import org.specs2.mutable.Specification


class UtilSpec  extends Specification with TestResources  {

  "Util" should {
    val base = resourcePath("integrityCheck")
    
    "check containAll for list" in {
      val l1 = List(1,2,3,4)
      val l2 = List(1,2,3)
      
      Util.containAll(l1,l2) mustEqual true
      Util.containAll(l2,l1) mustEqual false
    }
    
    "list file in folder" in {

      val apiFiles = Util.findAllFiles(new File(acceptancePath))
      
      apiFiles must haveLength(65)
      
      apiFiles must contain (new File(s"$basePath/uk/gov/nationalarchives/csv/validator/acceptance/twoRulesPassMetaData.csv"))

      apiFiles must contain (new File(s"$basePath/uk/gov/nationalarchives/csv/validator/acceptance/dp/regexRuleSchema.csvs"))
      
      apiFiles must contain (new File(s"$basePath/uk/gov/nationalarchives/csv/validator/acceptance/dp"))
      
      apiFiles must contain (new File(s"$basePath/uk/gov/nationalarchives/csv/validator/acceptance"))
      
      val integrityCheckFiles =  Util.findAllFiles(new File(base))


      integrityCheckFiles  must haveLength(17)

      integrityCheckFiles must contain (new File(s"$basePath/uk/gov/nationalarchives/csv/validator/integrityCheck/header/integrityCheckSchema.csvs"))

      integrityCheckFiles must contain (new File(s"$basePath/uk/gov/nationalarchives/csv/validator/integrityCheck/header/content/file1"))

      integrityCheckFiles must contain (new File(s"$basePath/uk/gov/nationalarchives/csv/validator/integrityCheck/header/content"))
     
      integrityCheckFiles must contain (new File(s"$basePath/uk/gov/nationalarchives/csv/validator/integrityCheck"))


    }
  }
}
