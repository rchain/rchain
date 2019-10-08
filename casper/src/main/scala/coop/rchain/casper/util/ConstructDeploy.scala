package coop.rchain.casper.util

import coop.rchain.models.PCost
import cats.implicits._
import cats.{Functor, Monad}
import com.google.protobuf.ByteString
import coop.rchain.casper.SignDeployment
import coop.rchain.casper.protocol.{DeployData, ProcessedDeploy, ProcessedDeployProto}
import coop.rchain.crypto.PrivateKey
import coop.rchain.crypto.codec.Base16
import coop.rchain.crypto.signatures.Secp256k1
import coop.rchain.shared.Time

object ConstructDeploy {

  val defaultSec = PrivateKey(
    Base16.unsafeDecode("a68a6e6cca30f81bd24a719f3145d20e8424bd7b396309b0708a16c7d8000b76")
  )
  val defaultPub     = Secp256k1.toPublic(defaultSec)
  val defaultKeyPair = (defaultSec, defaultPub)

  val defaultSec2 = PrivateKey(
    Base16.unsafeDecode("1111111111111111111111111111111111111111111111111111111111111111")
  )
  val defaultPub2     = Secp256k1.toPublic(defaultSec2)
  val defaultKeyPair2 = (defaultSec2, defaultPub2)

  def sign(deploy: DeployData, sec: PrivateKey = defaultSec): DeployData =
    SignDeployment.sign(sec, deploy, Secp256k1)

  def sourceDeploy(
      source: String,
      timestamp: Long,
      phloLimit: Long = 90000,
      phloPrice: Long = 1L,
      sec: PrivateKey = defaultSec
  ): DeployData = {
    val data =
      DeployData(
        deployer = ByteString.copyFrom(Secp256k1.toPublic(sec).bytes),
        term = source,
        timestamp = timestamp,
        sig = ByteString.EMPTY,
        sigAlgorithm = "",
        phloLimit = phloLimit,
        phloPrice = phloPrice,
        validAfterBlockNumber = 0L
      )

    sign(data, sec)
  }

  def sourceDeployNow(
      source: String
  ): DeployData =
    sourceDeploy(source = source, timestamp = System.currentTimeMillis())

  def sourceDeployNowF[F[_]: Time: Functor](
      source: String,
      phloLimit: Long = 90000,
      phloPrice: Long = 1L,
      sec: PrivateKey = defaultSec
  ): F[DeployData] =
    Time[F].currentMillis
      .map(sourceDeploy(source, _, phloLimit = phloLimit, phloPrice = phloPrice, sec = sec))

  // TODO: replace usages with basicSendDeployData
  def basicDeployData[F[_]: Monad: Time](
      id: Int
  ): F[DeployData] =
    sourceDeployNowF(source = s"@$id!($id)")

  def basicSendDeployData[F[_]: Monad: Time](
      id: Int
  ): F[DeployData] = basicDeployData[F](id)

  def basicReceiveDeployData[F[_]: Monad: Time](
      id: Int
  ): F[DeployData] =
    sourceDeployNowF(source = s"for(_ <- @$id){ Nil }")

  def basicProcessedDeploy[F[_]: Monad: Time](
      id: Int
  ): F[ProcessedDeploy] =
    basicDeployData[F](id).map(
      deploy => ProcessedDeploy(deploy = deploy, cost = PCost(0L), List.empty, List.empty, false)
    )
}
