package coop.rchain.catscontrib

import cats._, cats.data._, cats.implicits._
import Catscontrib._

trait Encryption[F[_]] {
  import Encryption._
  def generateNonce: F[Nonce]
}

object Encryption extends EncryptionInstances {

  type Nonce    = Array[Byte]
  type Key      = Array[Byte]
  type Messatge = Array[Byte]

  def apply[F[_]](implicit L: Encryption[F]): Encryption[F] = L

  def forTrans[F[_]: Monad, T[_[_], _]: MonadTrans](
      implicit E: Encryption[F]): Encryption[T[F, ?]] =
    new Encryption[T[F, ?]] {

      def generateNonce: T[F, Nonce] = E.generateNonce.liftM[T]
    }
}

sealed abstract class EncryptionInstances {
  implicit def eitherTEncryption[E, F[_]: Monad: Encryption[?[_]]]: Encryption[EitherT[F, E, ?]] =
    Encryption.forTrans[F, EitherT[?[_], E, ?]]
}
