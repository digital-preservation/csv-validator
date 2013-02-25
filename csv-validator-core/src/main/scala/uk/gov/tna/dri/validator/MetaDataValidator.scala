package uk.gov.tna.dri.validator

import scalaz.Scalaz._
import java.io.Reader
import uk.gov.tna.dri.schema.Schema
import scalaz.{Applicative, Traverse, CanBuildAnySelf}

trait MetaDataValidator {
  type MetaDataValidation[S] = ValidationNEL[String, S]

  def validate(csv: Reader, schema: Schema): MetaDataValidation[Any]
}