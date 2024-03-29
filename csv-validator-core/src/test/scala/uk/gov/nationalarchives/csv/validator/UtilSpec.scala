/*
 * Copyright (c) 2013, The National Archives <digitalpreservation@nationalarchives.gov.uk>
 * https://www.nationalarchives.gov.uk
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package uk.gov.nationalarchives.csv.validator

import org.junit.runner.RunWith
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner

import java.nio.file.Paths

@RunWith(classOf[JUnitRunner])
class UtilSpec extends Specification with TestResources  {

  "Util" should {
    val base = resourcePath("integrityCheck")
    
    "check containAll for list" in {
      val l1 = List(1,2,3,4)
      val l2 = List(1,2,3)
      
      Util.containAll(l1,l2) mustEqual true
      Util.containAll(l2,l1) mustEqual false
    }


    "get set A minus set B" in {
      val l1 = Set(1,2,3,4)
      val l2 = Set(1,2,3)
      val l3 = Set(1,2,3,5)

      Util.minus(l1,l2) mustEqual Set(4)
      Util.minus(l2,l1) mustEqual Set()
      Util.minus(l1,l3) mustEqual Set(4)
    }

    "get difference between set A and set B" in {
      val l1 = Set(1,2,3,4)
      val l2 = Set(1,2,3)
      val l3 = Set(1,2,3,5)

      Util.diff(l1,l2) mustEqual Set(4)
      Util.diff(l2,l1) mustEqual Set(4)
      Util.diff(l1,l3) mustEqual Set(4,5)
    }
    
    "list file in folder" in {

      val apiFiles = Util.findAllFiles(true, Paths.get(acceptancePath))

      apiFiles must haveLength(127)

      apiFiles must contain (Paths.get(s"$basePath/uk/gov/nationalarchives/csv/validator/acceptance/twoRulesPassMetaData.csv"))

      apiFiles must contain (Paths.get(s"$basePath/uk/gov/nationalarchives/csv/validator/acceptance/dp/regexRuleSchema.csvs"))

      apiFiles must contain (Paths.get(s"$basePath/uk/gov/nationalarchives/csv/validator/acceptance/dp"))

      apiFiles must contain (Paths.get(s"$basePath/uk/gov/nationalarchives/csv/validator/acceptance"))

      val integrityCheckFiles =  Util.findAllFiles(true, Paths.get(base))

      integrityCheckFiles  must haveLength(43)

      integrityCheckFiles must contain (Paths.get(s"$basePath/uk/gov/nationalarchives/csv/validator/integrityCheck/header/integrityCheckSchema.csvs"))

      integrityCheckFiles must contain (Paths.get(s"$basePath/uk/gov/nationalarchives/csv/validator/integrityCheck/header/content/file1"))

      integrityCheckFiles must contain (Paths.get(s"$basePath/uk/gov/nationalarchives/csv/validator/integrityCheck/header/content"))


      integrityCheckFiles must contain (Paths.get(s"$basePath/uk/gov/nationalarchives/csv/validator/integrityCheck/noheader/content"))

      integrityCheckFiles must contain (Paths.get(s"$basePath/uk/gov/nationalarchives/csv/validator/integrityCheck/WO_95/content"))

      integrityCheckFiles must contain (Paths.get(s"$basePath/uk/gov/nationalarchives/csv/validator/integrityCheck"))

      val integrityCheckFilesNoFolder =  Util.findAllFiles(false, Paths.get(base))

      integrityCheckFilesNoFolder  must haveLength(29)

      integrityCheckFilesNoFolder must contain (Paths.get(s"$basePath/uk/gov/nationalarchives/csv/validator/integrityCheck/header/content/file1"))

      integrityCheckFilesNoFolder must contain (Paths.get(s"$basePath/uk/gov/nationalarchives/csv/validator/integrityCheck/header/integrityCheckSchema.csvs"))

      integrityCheckFilesNoFolder must not contain (Paths.get(s"$basePath/uk/gov/nationalarchives/csv/validator/integrityCheck/noheader/content"))

      integrityCheckFilesNoFolder must not contain (Paths.get(s"$basePath/uk/gov/nationalarchives/csv/validator/integrityCheck/WO_95/content"))


    }
  }
}
