package coop.rchain.casper

import coop.rchain.crypto.signatures.Ed25519
import coop.rchain.crypto.codec.Base16
import coop.rchain.crypto.{PrivateKey, PublicKey}
import coop.rchain.casper.protocol._
import coop.rchain.rholang.interpreter.accounting
import coop.rchain.shared.{Log, LogSource, Time}
import com.google.protobuf.{ByteString, Int32Value, StringValue}
import cats._, cats.data._, cats.implicits._

object DeployGenerator {

  val sec = PrivateKey(
    Base16.decode("b18e1d0045995ec3d010c387ccfeb984d783af8fbb0f40fa7db126d889f6dadd")
  )

  val pub = Ed25519.toPublic(sec)

  private def sign(deploy: DeployData): DeployData =
    SignDeployment(sec, deploy, Ed25519)

  def sourceDeploy(source: String, timestamp: Long, phlos: Long): DeployData =
    sign(
      DeployData(
        user = ByteString.copyFrom(pub.bytes),
        timestamp = timestamp,
        term = source,
        phloLimit = phlos
      )
    )

  def sourceDeployNow(source: String): DeployData =
    sourceDeploy(
      source,
      System.currentTimeMillis(),
      accounting.MAX_VALUE
    )

  def basicDeployData[F[_]: Monad: Time](id: Int): F[DeployData] =
    Time[F].currentMillis.map { now =>
      sign(
        DeployData()
          .withUser(ByteString.copyFrom(pub.bytes))
          .withTimestamp(now)
          .withTerm(s"@${id}!($id)")
          .withPhloLimit(accounting.MAX_VALUE)
      )
    }

  def basicProcessedDeploy[F[_]: Monad: Time](id: Int): F[ProcessedDeploy] =
    basicDeployData[F](id).map(deploy => ProcessedDeploy(deploy = Some(deploy)))
}
