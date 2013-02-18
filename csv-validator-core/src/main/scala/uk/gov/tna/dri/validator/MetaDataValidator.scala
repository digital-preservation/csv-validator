package uk.gov.tna.dri.validator

import scalaz.Scalaz._
import java.io.Reader
import uk.gov.tna.dri.schema.Schema
import scalaz.{Applicative, Traverse, CanBuildAnySelf}

trait MetaDataValidator {

  type MetaDataValidation[S] = ValidationNEL[String, S]

  def validate(csv: Reader, schema: Schema): MetaDataValidation[Any]

  /** TODO Really want to use traverseSTrampoline */
  implicit def TraversableTraverse[CC[X] <: collection.SeqLike[X, CC[X]] : CanBuildAnySelf] = new Traverse[CC] {
    def traverse[F[_]: Applicative, A, B](f: A => F[B], as: CC[A]): F[CC[B]] = {
      val cbf = implicitly[CanBuildAnySelf[CC]].builder[B, B]
      (Vector.empty[B].pure[F] /: as)((bs, a) => (bs <**> f(a))(_ :+ _)) map (v => (cbf.apply() ++= v).result)
    }
  }
}