package coop.rchain.comm

import coop.rchain.catscontrib.Capture
import com.roundeights.hasher.Implicits._
import com.google.common.io.BaseEncoding

import cats._, cats.data._, cats.implicits._

final case class PublicPrivateKeys(pub: Array[Byte], priv: Array[Byte])

object encryption {

  /** FIX-ME this should use finctionality from cryptography subproject
    * once it is here, see https://github.com/rchain/rchain/pull/247/filescode
    * for detail
    */
  def generate: PublicPrivateKeys =
    PublicPrivateKeys(hashIt("public"), hashIt("private"))

  /** FIX-ME This will sign any hashed 32 byte message */
  /** TODO secp256k1 requires message to be 32 bytes, this should be checked and
    * visible in type signature
    */
  def sign(priv: Array[Byte], hash: Array[Byte]): Array[Byte] = hash

  val hashIt: String => Array[Byte] = (str: String) =>
    BaseEncoding.base16().lowerCase.decode(str.sha256.hex)

  /** FIX-ME this should store public and private keys on disk or internal DB */
  def storeKeys[F[_]: Capture: Applicative](keys: PublicPrivateKeys): F[Unit] = ().pure[F]

  /** FIX-ME this should check if public and priavate keys are available on internal DB */
  def keysAvailable[F[_]: Capture: Applicative] = false.pure[F]

  /** FIX-ME this should retrive public and private keys from disk or internal DB */
  def fetchKeys[F[_]: Capture](
      implicit err: ApplicativeError[F, CommError]
  ): F[PublicPrivateKeys] = err.raiseError(KeysNotAvailable)
}
