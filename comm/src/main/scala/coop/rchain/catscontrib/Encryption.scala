package coop.rchain.catscontrib

import cats._, cats.data._, cats.implicits._
import Catscontrib._

trait Encryption[F[_]] {
  import Encryption._
  def fetchKeys: F[PublicPrivateKeys]
  def generateNonce: F[Nonce]
  def encrypt(pub: Key, sec: Key, nonce: Nonce, message: Array[Byte]): F[Array[Byte]]
  def decrypt(pub: Key, sec: Key, nonce: Nonce, cipher: Array[Byte]): F[Array[Byte]]
}

final case class PublicPrivateKeys(pub: Encryption.Key, priv: Encryption.Key)

object Encryption extends EncryptionInstances {

  type Nonce    = Array[Byte]
  type Key      = Array[Byte]
  type Messatge = Array[Byte]

  def apply[F[_]](implicit L: Encryption[F]): Encryption[F] = L

  def forTrans[F[_]: Monad, T[_[_], _]: MonadTrans](
      implicit E: Encryption[F]): Encryption[T[F, ?]] =
    new Encryption[T[F, ?]] {
      def fetchKeys: T[F, PublicPrivateKeys] = E.fetchKeys.liftM[T]
      def generateNonce: T[F, Nonce]         = E.generateNonce.liftM[T]
      def encrypt(pub: Key, sec: Key, nonce: Nonce, message: Array[Byte]): T[F, Array[Byte]] =
        E.encrypt(pub, sec, nonce, message).liftM[T]
      def decrypt(pub: Key, sec: Key, nonce: Nonce, cipher: Array[Byte]): T[F, Array[Byte]] =
        E.decrypt(pub, sec, nonce, cipher).liftM[T]
    }
}

sealed abstract class EncryptionInstances {
  implicit def eitherTEncryption[E, F[_]: Monad: Encryption[?[_]]]: Encryption[EitherT[F, E, ?]] =
    Encryption.forTrans[F, EitherT[?[_], E, ?]]
}
