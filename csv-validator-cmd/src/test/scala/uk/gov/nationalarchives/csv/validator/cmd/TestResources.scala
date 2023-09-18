/*
 * Copyright (c) 2013, The National Archives <digitalpreservation@nationalarchives.gov.uk>
 * https://www.nationalarchives.gov.uk
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package uk.gov.nationalarchives.csv.validator.cmd

import org.specs2.mutable.Specification
import uk.gov.nationalarchives.csv.validator.FILE_SEPARATOR

import java.nio.file.Paths

trait TestResources {

  spec: Specification =>

  /**
   * Utility function to get an absolute path to a resource
   * in the same class as a spec test
   *
   * @param resource The path of the resource relative to the spec test
   */
  def resourcePath(resource: String) : String = Paths.get(baseResourcePkgPath).resolve(resource).toAbsolutePath.toString

  def baseResourcePkgPath : String  = Paths.get(basePath).resolve(spec.getClass.getPackage.getName.replace('.', '/')).toAbsolutePath.toString

  def basePath : String = {
    val url = spec.getClass.getClassLoader.getResource(".")
    Paths.get(url.toURI).toAbsolutePath.toString
  }

  def relBasePath : String = basePath.replace(System.getProperty("user.dir") + FILE_SEPARATOR, "")
  def relBaseResourcePkgPath : String = Paths.get(relBasePath).resolve(spec.getClass.getPackage.getName.replace('.', '/')).toString
  def relResourcePath(resource: String) : String = Paths.get(relBaseResourcePkgPath).resolve(resource).toString
}
