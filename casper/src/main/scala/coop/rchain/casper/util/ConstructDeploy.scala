package coop.rchain.casper.util

import cats.implicits._
import cats.{Functor, Monad}
import com.google.protobuf.ByteString
import coop.rchain.casper.SignDeployment
import coop.rchain.casper.protocol.{DeployData, ProcessedDeploy}
import coop.rchain.crypto.PrivateKey
import coop.rchain.crypto.codec.Base16
import coop.rchain.crypto.signatures.Secp256k1
import coop.rchain.shared.Time

object ConstructDeploy {

  val defaultSec = PrivateKey(
    Base16.unsafeDecode("a68a6e6cca30f81bd24a719f3145d20e8424bd7b396309b0708a16c7d8000b76")
  )

  def sign(deploy: DeployData, sec: PrivateKey = defaultSec): DeployData =
    SignDeployment.sign(sec, deploy, Secp256k1)

  def sourceDeploy(
      source: String,
      timestamp: Long,
      phloLimit: Long = 9000,
      phloPrice: Long = 1L,
      sec: PrivateKey = defaultSec
  ): DeployData = {
    val data = DeployData(
      deployer = ByteString.copyFrom(Secp256k1.toPublic(sec).bytes),
      timestamp = timestamp,
      term = source,
      phloLimit = phloLimit,
      phloPrice = phloPrice
    )
    sign(data, sec)
  }

  def sourceDeployNow(
      source: String
  ): DeployData =
    sourceDeploy(source = source, timestamp = System.currentTimeMillis())

  def sourceDeployNow(
      source: String,
      phloLimit: Long = 90000,
      phloPrice: Long = 1L,
      sec: PrivateKey = defaultSec
  ): DeployData =
    sourceDeploy(
      source = source,
      timestamp = System.currentTimeMillis(),
      phloLimit = phloLimit,
      phloPrice = phloPrice,
      sec = sec
    )

  def sourceDeployNowF[F[_]: Time: Functor](
      source: String,
      phloLimit: Long = 90000,
      phloPrice: Long = 1L,
      sec: PrivateKey = defaultSec
  ): F[DeployData] =
    Time[F].currentMillis
      .map(sourceDeploy(source, _, phloLimit = phloLimit, phloPrice = phloPrice, sec = sec))

  def sourceDeployNowF[F[_]: Time: Functor](source: String): F[DeployData] =
    Time[F].currentMillis.map(sourceDeploy(source, _))

  def basicDeployData[F[_]: Monad: Time](
      id: Int,
      sec: PrivateKey = defaultSec
  ): F[DeployData] =
    sourceDeployNowF(source = s"$id!($id)", sec = sec)

  def basicProcessedDeploy[F[_]: Monad: Time](
      id: Int,
      sec: PrivateKey = defaultSec
  ): F[ProcessedDeploy] =
    basicDeployData[F](id = id, sec = sec).map(deploy => ProcessedDeploy(deploy = Some(deploy)))
}
