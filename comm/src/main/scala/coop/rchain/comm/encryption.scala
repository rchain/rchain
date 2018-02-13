package coop.rchain.comm

import coop.rchain.catscontrib.Capture

import cats._, cats.data._, cats.implicits._
import monix.eval.Task

final case class PublicPrivateKeys(pub: Array[Byte], priv: Array[Byte])

object PublicPrivateKeys {

  /** TODO this should use finctionality from cryptography subproject
    * once it is here, see https://github.com/rchain/rchain/pull/247/filescode
    * for detail
    */
  def generate: PublicPrivateKeys = {

    import com.roundeights.hasher.Implicits._
    import com.google.common.io.BaseEncoding

    val to32ByteArr: String => Array[Byte] =
      (str: String) => BaseEncoding.base16().decode(str.sha256)

    PublicPrivateKeys(to32ByteArr("public"), to32ByteArr("private"))
  }

  /** FIX-ME this should store public and private keys on disk or internal DB */
  def storeKeys[F[_]: Capture: Applicative](keys: PublicPrivateKeys): F[Unit] = ().pure[F]

  /** FIX-ME this should check if public and priavate keys are available on internal DB */
  def keysAvailable[F[_]: Capture: Applicative] = false.pure[F]

  /** FIX-ME this should retrive public and private keys from disk or internal DB */
  def fetchKeys[F[_]: Capture](
      implicit err: ApplicativeError[F, CommError]
  ): F[PublicPrivateKeys] = err.raiseError(KeysNotAvailable)
}
