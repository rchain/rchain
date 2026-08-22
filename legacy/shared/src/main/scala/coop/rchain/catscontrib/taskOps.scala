package coop.rchain.catscontrib

import cats.effect.Sync
import cats.syntax.all._
import coop.rchain.shared.{Log, LogSource}

object TaskContrib {

  implicit class AbstractTaskOps[F[_], A](val fa: F[A]) extends AnyVal {

    // TODO: Migrated from previous Task version `attemptAndLog` / do we really need it?
    def logOnError(msg: String)(implicit s: Sync[F], log: Log[F], ev: LogSource): F[A] =
      fa.onError { case ex => log.error(msg, ex) }

  }
}
