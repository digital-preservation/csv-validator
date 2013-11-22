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

trait TestResources {

  spec: Specification =>

  /**
   * Utility function to get an absolute path to a resource
   * in the same class as a spec test
   *
   * @param resource The path of the resource relative to the spec test
   */
  def resourcePath(resource: String) : String = new File(baseResourcePkgPath, resource).getAbsolutePath

  def baseResourcePkgPath : String = new File(basePath, spec.getClass.getPackage.getName.replace('.', '/')).getAbsolutePath

  def basePath : String = {
    val url = spec.getClass.getClassLoader.getResource(".")
    new File(url.toURI).getAbsolutePath
  }

  def relBasePath : String = basePath.replace(System.getProperty("user.dir") + FILE_SEPARATOR, "")
  def relBaseResourcePkgPath : String = new File(relBasePath, spec.getClass.getPackage.getName.replace('.', '/')).getPath
  def relResourcePath(resource: String) : String = new File(relBaseResourcePkgPath, resource).getPath

  val acceptancePath = resourcePath("acceptance")

  val threeFilesInSubDirPath = resourcePath("fileCountTestFiles/threeFilesinSubDir")
  val threeFilesPath = resourcePath("fileCountTestFiles/threeFiles")

  val schemaPath = resourcePath("schema")
  val checksumPath = resourcePath("schema/checksum.csvs")
}
