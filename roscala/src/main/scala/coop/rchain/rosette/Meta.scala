package coop.rchain.rosette

import cats.data.{Kleisli, ReaderT}
import cats.MonadError
import cats.implicits._
import coop.rchain.rosette.Location.valWrt
import coop.rchain.rosette.utils.implicits.LiftOps

object Meta {
  case class StdMeta(meta: Ob, parent: Ob, override val extension: StdExtension) extends Actor {
    lazy val map = extension.slot.head.asInstanceOf[RblTable]

    def keyLoc(key: Ob, client: Ob): Location = {
      val atom = map.getKey(key)

      if (atom == Ob.ABSENT) {
        Limbo
      } else {
        atom.asInstanceOf[Location]
      }
    }

    def get[F[_]](client: Ob, key: Ob)(
        implicit E: MonadError[F, RblError]
    ): ReaderT[F, GlobalEnv, Ob] = {
      val container = client

      def extension(ob: Ob, indirect: Boolean): Option[Ob] =
        Some(ob)
          .filter(_ => indirect)
          .map(_.asInstanceOf[Actor].extension)

      keyLoc(key, client) match {
        case LexVariable(indirect, _, offset) =>
          Kleisli.pure[F, GlobalEnv, Ob](
            extension(container, indirect)
              .getOrElse(container)
              .slot(offset))
        case Limbo =>
          Absent.liftE[RblError]
        case location =>
          valWrt[F](location, client)
      }
    }

    def lookupOBOStdMeta[F[_]](client: Ob, key: Ob)(
        implicit E: MonadError[F, RblError]): ReaderT[F, GlobalEnv, Ob] =
      get[F](client, key) recoverWith {
        case Absent => client.parent.lookup[F](key)
      }
  }

  object StdMeta {
    def apply(extension: StdExtension = null): StdMeta =
      StdMeta(meta = null, parent = null, extension)
  }
}
