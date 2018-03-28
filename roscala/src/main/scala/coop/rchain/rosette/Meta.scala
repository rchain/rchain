package coop.rchain.rosette

import cats.data.StateT
import cats.implicits._
import cats.{Applicative, MonadError}
import coop.rchain.rosette.Location.valWrt
import coop.rchain.rosette.utils.implicits.LiftOps

object Meta {
  def inspect[F[_]: Applicative, A](f: VMState => A) = StateT.inspect[F, VMState, A](f)
  def pure[F[_]: Applicative](ob: Ob)                = StateT.pure[F, VMState, Ob](ob)

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
    ): StateT[F, VMState, Ob] = {
      val container = client

      def extension(ob: Ob, indirect: Boolean): Option[Ob] =
        Some(ob)
          .filter(_ => indirect)
          .map(_.asInstanceOf[Actor].extension)

      def matchLocation(globalEnv: TblObject): Location => StateT[F, VMState, Ob] = {
        case LexVariable(indirect, _, offset) =>
          pure[F](
            extension(container, indirect)
              .getOrElse(container)
              .slot(offset))
        case Limbo =>
          Absent.liftE[F, RblError]
        case location =>
          StateT.liftF[F, VMState, Ob](valWrt[F](location, client).run(globalEnv))
      }

      val loc = keyLoc(key, client)

      for {
        globalEnv <- inspect[F, TblObject](_.globalEnv)
        res       <- matchLocation(globalEnv)(loc)
      } yield res
    }

    def lookupOBOStdMeta[F[_]](client: Ob, key: Ob)(
        implicit E: MonadError[F, RblError]): StateT[F, VMState, Ob] = {
      def getValue: StateT[F, VMState, Ob] =
        for {
          value <- get[F](client, key)
        } yield value

      for {
        interruptPending <- inspect[F, Boolean](_.interruptPending)
        res <- if (interruptPending) Absent.liftE[F, RblError]: StateT[F, VMState, Ob]
              else
                getValue.recoverWith {
                  case Absent =>
                    client.parent.lookup[F](key)
                }
      } yield res
    }
  }

  object StdMeta {
    def apply(extension: StdExtension = null): StdMeta =
      StdMeta(meta = null, parent = null, extension)
  }
}
