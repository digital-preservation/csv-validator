package uk.gov.tna.dri.io

import java.util.Date

/**
 * @tparam A
 * @author David Ainslie
 */
trait Resource[A] {
  /**
   * Whether this resource actually exists in physical form.
   * @return Boolean true by default i.e. if this resource has been instantiated it is expected to exist.
   */
  def exists: Boolean = true

  /**
   * Whether the contents of this resource can be read.
   * @return Boolean true by default i.e. if this resource has been instantiated it is expected to be readable.
   */
  def readable: Boolean = true

  /**
   * Content length for this resource.
   * @return Int content length e.g. file size.
   */
  def contentLength: Int = contents.size

  /**
   * Determine the last modified timestamp for this resource.
   * @return Option[java.util.Date] defaults to None e.g. maybe the resource has no concept of last modified (cannot save that information).
   */
  def lastModified: Option[Date] = None

  /**
   * Actual contents as an iterator of required type.
   * @return Iterator[A] the contents which only a concrete implementation of this (abstract) trait can define.
   */
  def contents: Iterator[A]
}
