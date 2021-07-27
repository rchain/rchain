package coop.rchain.node.web

import cats.effect.concurrent.Deferred
import cats.effect.{Concurrent, Sync}
import cats.syntax.all._
import com.google.protobuf.ByteString
import coop.rchain.casper.api.BlockReportAPI
import coop.rchain.casper.genesis.contracts.StandardDeploys
import coop.rchain.casper.util.rholang.Tools
import coop.rchain.models.GUnforgeable.UnfInstance.GPrivateBody
import coop.rchain.models.{GPrivate, GUnforgeable}
import coop.rchain.casper.protocol.{
  CloseBlockSystemDeployDataProto,
  DeployInfoWithEventData,
  ReportCommProto,
  ReportProduceProto,
  SingleReport,
  SlashSystemDeployDataProto
}
import coop.rchain.crypto.codec.Base16
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.models.Par
import coop.rchain.node.encode.JsonEncoder.convertCcodecToScodec
import coop.rchain.node.web.Transaction.TransactionStore
import coop.rchain.rspace.hashing.Blake2b256Hash
import coop.rchain.store.{KeyValueStoreManager, KeyValueTypedStore}
import coop.rchain.shared.syntax._
import scodec.Codec
import scodec.codecs.utf8

import scala.collection.concurrent.TrieMap
import scala.language.higherKinds

final case class Transaction(
    fromAddr: String,
    toAddr: String,
    amount: Long,
    retUnforgeable: Par,
    failReason: Option[String]
)

sealed trait TransactionType
final case class PreCharge(deployId: String)       extends TransactionType
final case class UserDeploy(deployId: String)      extends TransactionType
final case class Refund(deployId: String)          extends TransactionType
sealed trait SystemTransaction                     extends TransactionType
final case class CloseBlock(blockHash: String)     extends SystemTransaction
final case class SlashingDeploy(blockHash: String) extends SystemTransaction

final case class TransactionInfo(transaction: Transaction, transactionType: TransactionType)

final case class TransactionResponse(data: Seq[TransactionInfo])

trait TransactionAPI[F[_]] {
  def getTransaction(blockHash: Blake2b256Hash): F[Seq[TransactionInfo]]
}

/**
  * This API is totally based on how RevVault.rho is written. If the `RevVault.rho` is re-written or changed,
  * this API might end up with useless.
  */
final case class TransactionAPIImpl[F[_]: Concurrent](
    blockReportAPI: BlockReportAPI[F],
    // The transferUnforgeable can be retrieved based on the deployer and the timestamp of RevVault.rho
    // in the genesis ceremony.
    transferUnforgeable: Par
) extends TransactionAPI[F] {
  // Helper which accepts TransactionType constructor
  private def makeDeployTransaction(
      d: DeployInfoWithEventData,
      report: SingleReport,
      txCtor: String => TransactionType
  ) = findTransactions(report).map(t => TransactionInfo(t, txCtor(d.deployInfo.get.sig)))

  def getTransaction(blockHash: Blake2b256Hash): F[Seq[TransactionInfo]] =
    blockReportAPI.blockReport(blockHash.toByteString, forceReplay = false).map {
      case Right(b) =>
        val userTransactions = b.deploys.flatMap(d => {
          d.report
            .zip(Seq(PreCharge, UserDeploy, Refund))
            .flatMap {
              case (report, txCtor) =>
                makeDeployTransaction(d, report, txCtor)
            }
        })
        val systemDeployTransactions = b.systemDeploys.flatMap { s =>
          // system doesn't get precharge and refund , so it would always get one
          val transactions = findTransactions(s.report.head)
          val txCtor = s.systemDeploy.get.systemDeploy.value match {
            case SlashSystemDeployDataProto(_, _)  => SlashingDeploy
            case CloseBlockSystemDeployDataProto() => CloseBlock
          }
          transactions.map(t => TransactionInfo(t, txCtor(Base16.encode(blockHash.bytes.toArray))))
        }
        userTransactions ++ systemDeployTransactions

      case Left(_) => Seq.empty
    }

  private def findTransactions(report: SingleReport): Seq[Transaction] = {
    val transactions = report.events
      .map(_.report.value)
      .collect {
        case ReportCommProto(Some(consume), produces) =>
          consume.channels.headOption.map((_, produces))
      }
      .flatten
      .collect { case (channel, produces) if channel == transferUnforgeable => produces.head }
      .map { produce =>
        val fromAddr       = produce.data.get.pars.head.exprs.head.getGString
        val toAddr         = produce.data.get.pars(2).exprs.head.getGString
        val amount         = produce.data.get.pars(3).exprs.head.getGInt
        val retUnforgeable = produce.data.get.pars(5)
        Transaction(fromAddr, toAddr, amount, retUnforgeable, None)
      }

    val transactionRetUnforgeables = transactions.map(_.retUnforgeable).toSet

    val failedMap = report.events.iterator
      .map(_.report.value)
      .collect {
        case ReportProduceProto(Some(channel), data)
            if transactionRetUnforgeables.contains(channel) =>
          val success =
            data.get.pars.head.exprs.head.getETupleBody.ps.head.exprs.head.getGBool
          val failedReason =
            if (success) None
            else Some(data.get.pars.head.exprs.head.getETupleBody.ps(1).exprs.head.getGString)
          (channel, failedReason)
      }
      .toMap
    transactions.map { t =>
      val failReason = failedMap.getOrElse(t.retUnforgeable, None)
      t.copy(failReason = failReason)
    }
  }

}

final case class CacheTransactionAPI[F[_]: Sync: Concurrent](
    transactionAPI: TransactionAPI[F],
    store: TransactionStore[F]
) {

  private val blockDeferMap: TrieMap[String, Deferred[F, Unit]] = TrieMap.empty

  def getTransaction(blockHash: String): F[TransactionResponse] =
    for {
      defNew <- Deferred[F, Unit]
      defer  = blockDeferMap.getOrElseUpdate(blockHash, defNew)
      _ <- (for {
            transactions <- transactionAPI.getTransaction(Blake2b256Hash.fromHex(blockHash))
            _            <- store.put(blockHash, TransactionResponse(transactions))
            _            <- defer.complete(())
          } yield ()).whenA(defNew == defer)
      _               <- defer.get
      transactionsOpt <- store.get(blockHash)
      transactions    <- transactionsOpt.liftTo(new Exception(s"Transactions $blockHash not found."))
    } yield transactions
}

object Transaction {
  type TransactionStore[F[_]] = KeyValueTypedStore[F, String, TransactionResponse]

  object Encode {
    import io.circe._, io.circe.generic.semiauto._
    import coop.rchain.node.encode.JsonEncoder.{decodePar, encodePar}
    implicit val encodeTransaction: Encoder[Transaction]         = deriveEncoder[Transaction]
    implicit val encodeTransactionType: Encoder[TransactionType] = deriveEncoder[TransactionType]
    implicit val encodeTransactionInfo: Encoder[TransactionInfo] = deriveEncoder[TransactionInfo]
    implicit val encodeTransactionResponse: Encoder[TransactionResponse] =
      deriveEncoder[TransactionResponse]

    implicit val decodeTransaction: Decoder[Transaction]         = deriveDecoder[Transaction]
    implicit val decodeTransactionType: Decoder[TransactionType] = deriveDecoder[TransactionType]
    implicit val decodeTransactionInfo: Decoder[TransactionInfo] = deriveDecoder[TransactionInfo]
    implicit val decodeTransactionResponse: Decoder[TransactionResponse] =
      deriveDecoder[TransactionResponse]
  }

  def transactionResponseCodec: Codec[TransactionResponse] =
    convertCcodecToScodec(Encode.encodeTransactionResponse, Encode.decodeTransactionResponse)

  // This is the hard-coded unforgeable name for
  // https://github.com/rchain/rchain/blob/43257ddb7b2b53cffb59a5fe1d4c8296c18b8292/casper/src/main/resources/RevVault.rho#L25
  // This hard-coded value is only useful with current(above link version) `RevVault.rho` implementation but it is
  // useful for all the networks(testnet, custom network and mainnet) starting with this `RevVault.rho`.
  //
  // This hard-coded value needs to be changed when
  // 1. `RevVault.rho` is changed
  // 2. [[coop.rchain.casper.genesis.contracts.StandardDeploys.revVault]] is changed
  // 3. The random seed algorithm for unforgeable name of the deploy is changed
  //
  // This is not needed when onChain transfer history is implemented and deployed to new network in the future.
  val transferUnforgeable = {
    val seedForRevVault = Tools.unforgeableNameRng(
      StandardDeploys.revVault.pk,
      StandardDeploys.revVault.data.timestamp
    )
    // the 11th unforgeable name
    val unfogeableBytes = (1 to 11).foldLeft(Array.empty[Byte]) {
      case (_, _) => seedForRevVault.next()
    }
    GUnforgeable(GPrivateBody(GPrivate(ByteString.copyFrom(unfogeableBytes))))
  }

  def apply[F[_]: Sync: Concurrent](
      blockReportAPI: BlockReportAPI[F],
      // The transferUnforgeable can be retrieved based on the deployer and the timestamp of RevVault.rho
      // in the genesis ceremony.
      transferUnforgeable: Par
  ): TransactionAPIImpl[F] = TransactionAPIImpl(blockReportAPI, transferUnforgeable)

  def cacheTransactionAPI[F[_]: Sync: Concurrent](
      transactionAPI: TransactionAPI[F],
      kvm: KeyValueStoreManager[F]
  ): F[CacheTransactionAPI[F]] =
    kvm.database("transaction", utf8, transactionResponseCodec).map { s =>
      CacheTransactionAPI(
        transactionAPI,
        s
      )
    }
}
