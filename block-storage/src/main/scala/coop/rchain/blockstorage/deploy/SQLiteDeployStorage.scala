package coop.rchain.blockstorage.deploy

import cats.effect.Sync
import cats.instances.list._
import cats.syntax.functor._
import cats.syntax.applicative._
import com.google.protobuf.ByteString
import coop.rchain.casper.protocol.DeployData
import coop.rchain.crypto.PublicKey
import coop.rchain.crypto.signatures.{Secp256k1, Signed}
import doobie._
import doobie.implicits._

class SQLiteDeployStorage[F[_]: Sync](transactor: Transactor[F]) extends DeployStorage[F] {
  implicit protected val readDeployData: Read[Signed[DeployData]] =
    Read[(Array[Byte], Array[Byte], String, Long, Long, Long, Long)].map {
      case (signature, pk, term, timestamp, phloPrice, phloLimit, validAfterBlockNumber) =>
        val deployData = DeployData(term, timestamp, phloPrice, phloLimit, validAfterBlockNumber)
        Signed
          .fromSignedData(deployData, PublicKey(pk), ByteString.copyFrom(signature), Secp256k1)
          .get
    }

  implicit protected val writeDeployData: Write[Signed[DeployData]] =
    Write[(Array[Byte], Array[Byte], String, Long, Long, Long, Long)].contramap { deploy =>
      (
        deploy.sig.toByteArray,
        deploy.pk.bytes,
        deploy.data.term,
        deploy.data.timestamp,
        deploy.data.phloPrice,
        deploy.data.phloLimit,
        deploy.data.validAfterBlockNumber
      )
    }

  def put(deploys: List[Signed[DeployData]]): F[Unit] =
    Update[Signed[DeployData]](
      """INSERT OR IGNORE INTO deploys
        |(signature, public_key, term, timestamp, phlo_price, phlo_limit, valid_after_block_number)
        |VALUES (?, ?, ?, ?, ?, ?, ?)
        |""".stripMargin
    ).updateMany(deploys).transact(transactor).void

  def remove(deploys: List[Signed[DeployData]]): F[Int] =
    Update[Array[Byte]](
      "DELETE FROM deploys WHERE signature=?"
    ).updateMany(deploys.map(d => d.sig.toByteArray))
      .transact(transactor)

  def getUnfinalized: F[Set[Signed[DeployData]]] =
    sql"SELECT * FROM deploys"
      .query[Signed[DeployData]]
      .to[Set]
      .transact(transactor)
}

object SQLiteDeployStorage {
  def make[F[_]: Sync](transactor: Transactor[F]): F[DeployStorage[F]] =
    new SQLiteDeployStorage[F](transactor).pure[F].widen
}
