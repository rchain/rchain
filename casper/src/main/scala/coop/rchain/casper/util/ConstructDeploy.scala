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
      source: String,
      phloLimit: Long = 90000,
      phloPrice: Long = 1L,
      sec: PrivateKey = defaultSec
  ): DeployData =
    sourceDeploy(source, System.currentTimeMillis(), phloLimit, phloPrice, sec)

  def sourceDeployNowF[F[_]: Time: Functor](
      source: String,
      phloLimit: Long = 90000,
      phloPrice: Long = 1L,
      sec: PrivateKey = defaultSec
  ): F[DeployData] =
    Time[F].currentMillis.map(sourceDeploy(source, _, phloLimit, phloPrice, sec))

  def basicDeployData[F[_]: Monad: Time](
      id: Int,
      sec: PrivateKey = defaultSec,
      phlos: Long = 100
  ): F[DeployData] =
    Time[F].currentMillis.map { now =>
      val data = DeployData()
        .withDeployer(ByteString.copyFrom(Secp256k1.toPublic(sec).bytes))
        .withTimestamp(now)
        .withTerm(s"@$id!($id)")
        .withPhloLimit(phlos)
        .withPhloPrice(1)
      sign(data, sec)
    }

  def basicProcessedDeploy[F[_]: Monad: Time](
      id: Int,
      sec: PrivateKey = defaultSec
  ): F[ProcessedDeploy] =
    basicDeployData[F](id, sec).map(deploy => ProcessedDeploy(deploy = Some(deploy)))
}
