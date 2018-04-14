package coop.rchain.rosette

import coop.rchain.rosette.utils.implicits._
import cats.{Applicative, MonadError}

object RblTopEnv extends Ob {
  override val meta   = null
  override val parent = null

  override def lookup[F[_]: Applicative](key: Ob)(implicit E: MonadError[F, RblError]) =
    Absent.liftE[RblError]
}
