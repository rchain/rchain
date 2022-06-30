package coop.rchain.node.web

import cats.effect.concurrent.Deferred
import cats.effect.Concurrent
import cats.syntax.all._
import com.google.protobuf.ByteString
import coop.rchain.casper.BlockRandomSeed
import coop.rchain.casper.api.BlockReportApi
import coop.rchain.casper.genesis.Genesis
import coop.rchain.models.GPrivate
import coop.rchain.models.syntax._
import coop.rchain.casper.protocol.{
  CloseBlockSystemDeployDataProto,
  DeployInfoWithEventData,
  ReportCommProto,
  ReportProduceProto,
  SingleReport,
  SlashSystemDeployDataProto
}
import coop.rchain.casper.rholang.RuntimeManager.emptyStateHashFixed
import coop.rchain.crypto.PublicKey
import coop.rchain.crypto.hash.Blake2b512Random
import coop.rchain.models.Par
import coop.rchain.node.web.Transaction.TransactionStore
import coop.rchain.rholang.interpreter.RhoType.Name
import coop.rchain.rspace.hashing.Blake2b256Hash
import coop.rchain.shared.Base16
import coop.rchain.store.{KeyValueStoreManager, KeyValueTypedStore}
import coop.rchain.shared.syntax._
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
final case class TransactionResponse(data: List[TransactionInfo])

trait TransactionAPI[F[_]] {
  def getTransaction(blockHash: Blake2b256Hash): F[List[TransactionInfo]]
}

/**
  * This API is totally based on how RevVault.rho is written. If the `RevVault.rho` is re-written or changed,
  * this API might end up with useless.
  */
final case class TransactionAPIImpl[F[_]: Concurrent](
    blockReportAPI: BlockReportApi[F],
    // The transferUnforgeable can be retrieved based on the deployer and the timestamp of RevVault.rho
    // in the genesis ceremony.
    transferUnforgeable: Par
) extends TransactionAPI[F] {
  // Helper which accepts TransactionType constructor
  private def makeDeployTransaction(
      d: DeployInfoWithEventData,
      report: SingleReport,
      txCtor: String => TransactionType
  ) = findTransactions(report).map(t => TransactionInfo(t, txCtor(d.deployInfo.sig)))

  def getTransaction(blockHash: Blake2b256Hash): F[List[TransactionInfo]] =
    blockReportAPI.blockReport(blockHash.toByteString, forceReplay = false).flatMap { res =>
      res
        .traverse { b =>
          val userTransactions = b.deploys.toList.flatTraverse { d =>
            // FIXME https://github.com/rchain/rchain/issues/3150#issuecomment-701123669
            val zippedReport = d.report.length match {
              case 1 => d.report.zip(Seq(PreCharge)).pure // precharge fail with insufficient fund
              case 2 =>
                d.report
                  .zip(Seq(PreCharge, Refund))
                  .pure // user deploy doesn't generate logs like `new a in {}`
              case 3 => d.report.zip(Seq(PreCharge, UserDeploy, Refund)).pure // normal execution
              case _ =>
                new Exception(
                  s"It is not possible that user report ${d.deployInfo} amount is not equal to 1,2 or 3"
                ).raiseError[F, Seq[(SingleReport, String => TransactionType)]]
            }
            zippedReport
              .map(_.flatMap {
                case (report, txCtor) =>
                  makeDeployTransaction(d, report, txCtor)
              }.toList)
          }
          val systemDeployTransactions = b.systemDeploys.flatMap { s =>
            // system doesn't get precharge and refund , so it would always get one
            val transactions = findTransactions(s.report.head)
            val txCtor = s.systemDeploy.systemDeploy.value match {
              case SlashSystemDeployDataProto(_)     => SlashingDeploy
              case CloseBlockSystemDeployDataProto() => CloseBlock
            }
            transactions
              .map(t => TransactionInfo(t, txCtor(Base16.encode(blockHash.bytes.toArray))))
          }
          (userTransactions.map(_ ++ systemDeployTransactions))
        }
        .map(_.getOrElse(List.empty))
    }

  private def findTransactions(report: SingleReport): Seq[Transaction] = {
    val transactions = report.events.iterator
      .map(_.report.value)
      .collect {
        case ReportCommProto(consume, produces) =>
          consume.channels.headOption.map((_, produces))
      }
      .flatten
      .collect { case (channel, produces) if channel == transferUnforgeable => produces.head }
      .map { produce =>
        val fromAddr       = produce.data.pars.head.exprs.head.getGString
        val toAddr         = produce.data.pars(2).exprs.head.getGString
        val amount         = produce.data.pars(3).exprs.head.getGInt
        val retUnforgeable = produce.data.pars(5)
        Transaction(fromAddr, toAddr, amount, retUnforgeable, None)
      }
      .toList

    val transactionRetUnforgeables = transactions.map(_.retUnforgeable).toSet

    val failedMap = report.events.iterator
      .map(_.report.value)
      .collect {
        case ReportProduceProto(channel, data) if transactionRetUnforgeables.contains(channel) =>
          val success = data.pars.head.exprs.head.getETupleBody.ps.head.exprs.head.getGBool
          val failedReason =
            if (success) None
            else Some(data.pars.head.exprs.head.getETupleBody.ps(1).exprs.head.getGString)
          (channel, failedReason)
      }
      .toMap
    transactions.map { t =>
      val failReason = failedMap.getOrElse(t.retUnforgeable, None)
      t.copy(failReason = failReason)
    }
  }

}

final case class CacheTransactionAPI[F[_]: Concurrent](
    transactionAPI: TransactionAPI[F],
    store: TransactionStore[F]
) {

  private val blockDeferMap: TrieMap[String, Deferred[F, TransactionResponse]] = TrieMap.empty

  def getTransaction(blockHash: String): F[TransactionResponse] =
    store.get1(blockHash) >>= { transactionOpt =>
      transactionOpt.fold {
        for {
          defNew <- Deferred[F, TransactionResponse]
          defer  = blockDeferMap.getOrElseUpdate(blockHash, defNew)
          _ <- (for {
                transactions <- transactionAPI.getTransaction(
                                 Blake2b256Hash.fromHex(blockHash)
                               )
                transactionResp = TransactionResponse(transactions)
                _               <- store.put(blockHash, TransactionResponse(transactions))
                _               <- defer.complete(transactionResp)
              } yield transactionResp).whenA(defNew == defer)
          result <- defer.get
          // remove to reduce memory consumption
          _ = blockDeferMap.remove(blockHash)
        } yield result
      }(_.pure)
    }
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

  object SCodec {
    import scodec._
    import scodec.bits._
    import scodec.codecs._
    import coop.rchain.rholang.interpreter.storage._

    val transactionCodec: Codec[Transaction] =
      (utf8_32 :: utf8_32 :: vlong :: serializePar.toSizeHeadCodec :: optional[String](
        bool,
        utf8_32
      )).as[Transaction]
    val precharge: Codec[PreCharge]   = utf8_32.as[PreCharge]
    val refund: Codec[Refund]         = utf8_32.as[Refund]
    val user: Codec[UserDeploy]       = utf8_32.as[UserDeploy]
    val closeBlock: Codec[CloseBlock] = utf8_32.as[CloseBlock]
    val slash: Codec[SlashingDeploy]  = utf8_32.as[SlashingDeploy]
    val transactionType: Codec[TransactionType] = discriminated[TransactionType]
      .by(uint8)
      .subcaseP(0) {
        case e: PreCharge => e
      }(precharge)
      .subcaseP(1) {
        case s: UserDeploy => s
      }(user)
      .subcaseP(2) {
        case pb: Refund => pb
      }(refund)
      .subcaseP(3) {
        case c: CloseBlock => c
      }(closeBlock)
      .subcaseP(4) {
        case s: SlashingDeploy => s
      }(slash)

    val transactionInfo: Codec[TransactionInfo] =
      (transactionCodec :: transactionType).as[TransactionInfo]
    val transactionResponseCodec: Codec[TransactionResponse] = listOfN(int32, transactionInfo)
      .as[TransactionResponse]
  }

  // This is the unforgeable name for
  // https://github.com/rchain/rchain/blob/43257ddb7b2b53cffb59a5fe1d4c8296c18b8292/casper/src/main/resources/RevVault.rho#L25
  def transferUnforgeable(shardId: String): Par = {
    // RevVault contract is the 7th contract deployed in the genesis, start from 0. Index should be 6
    val RevVaultContractDeployIndex: Byte = 6
    val rand = BlockRandomSeed
      .generateSplitRandomNumber(
        BlockRandomSeed(
          shardId,
          Genesis.genesisRandomSeedBlockNumber,
          Genesis.genesisRandomSeedPubKey,
          emptyStateHashFixed.toBlake2b256Hash
        ),
        RevVaultContractDeployIndex,
        BlockRandomSeed.UserDeploySplitIndex
      )
    val unfogeableBytes = Iterator.continually(rand.next()).drop(10).next()
    Name(unfogeableBytes)
  }

  def apply[F[_]: Concurrent](
      blockReportAPI: BlockReportApi[F],
      // The transferUnforgeable can be retrieved based on the deployer and the timestamp of RevVault.rho
      // in the genesis ceremony.
      transferUnforgeable: Par
  ): TransactionAPIImpl[F] = TransactionAPIImpl(blockReportAPI, transferUnforgeable)

  def store[F[_]: Concurrent](
      kvm: KeyValueStoreManager[F]
  ): F[KeyValueTypedStore[F, String, TransactionResponse]] =
    kvm.database("transaction", utf8, SCodec.transactionResponseCodec)

  def cacheTransactionAPI[F[_]: Concurrent](
      transactionAPI: TransactionAPI[F],
      kvm: KeyValueStoreManager[F]
  ): F[CacheTransactionAPI[F]] =
    store(kvm).map { s =>
      CacheTransactionAPI(
        transactionAPI,
        s
      )
    }
}
