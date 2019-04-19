package coop.rchain.casper.util

import cats.Monad
import cats.implicits._
import com.google.protobuf.ByteString
import coop.rchain.casper.SignDeployment
import coop.rchain.casper.protocol.{DeployData, ProcessedDeploy}
import coop.rchain.crypto.PrivateKey
import coop.rchain.crypto.codec.Base16
import coop.rchain.crypto.signatures.Ed25519
import coop.rchain.rholang.interpreter.accounting
import coop.rchain.shared.Time

object ConstructDeploy {

  private val defaultSec = PrivateKey(
    Base16.unsafeDecode("b18e1d0045995ec3d010c387ccfeb984d783af8fbb0f40fa7db126d889f6dadd")
  )

  def sign(deploy: DeployData, sec: PrivateKey = defaultSec): DeployData =
    SignDeployment.sign(sec, deploy, Ed25519)

  def sourceDeploy(
      source: String,
      timestamp: Long,
      phlos: Long,
      phloPrice: Long = 0L,
      sec: PrivateKey = defaultSec
  ): DeployData = {
    val data = DeployData(
      deployer = ByteString.copyFrom(Ed25519.toPublic(sec).bytes),
      timestamp = timestamp,
      term = source,
      phloLimit = phlos,
      phloPrice = phloPrice
    )
    sign(data, sec)
  }

  def sourceDeployNow(source: String): DeployData =
    sourceDeploy(
      source,
      System.currentTimeMillis(),
      accounting.MAX_VALUE
    )

  def basicDeployData[F[_]: Monad: Time](
      id: Int,
      sec: PrivateKey = defaultSec,
      phlos: Long = accounting.MAX_VALUE
  ): F[DeployData] =
    Time[F].currentMillis.map { now =>
      val data = DeployData()
        .withDeployer(ByteString.copyFrom(Ed25519.toPublic(sec).bytes))
        .withTimestamp(now)
        .withTerm(s"@${id}!($id)")
        .withPhloLimit(phlos)
      sign(data, sec)
    }

  def basicProcessedDeploy[F[_]: Monad: Time](
      id: Int,
      sec: PrivateKey = defaultSec
  ): F[ProcessedDeploy] =
    basicDeployData[F](id, sec).map(deploy => ProcessedDeploy(deploy = Some(deploy)))
}
