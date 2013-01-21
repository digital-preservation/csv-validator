package uk.gov.tna.dri.schema

/**
 * Schema to parse/validate CSV resources.
 * @param name
 * @param totalColumns
 * @author David Ainslie
 */
case class Schema(val name: String, totalColumns: Int)
