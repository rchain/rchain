package coop.rchain.node.web

import cats.effect.concurrent.Deferred
import cats.effect.{Concurrent, Sync}
import cats.syntax.all._
import com.google.protobuf.ByteString
import coop.rchain.casper.api.BlockReportAPI
import coop.rchain.casper.genesis.contracts.StandardDeploys
import coop.rchain.casper.protocol.{CloseBlockSystemDeployDataProto, DeployInfoWithEventData, ReportCommProto, ReportProduceProto, SingleReport, SlashSystemDeployDataProto}
import coop.rchain.casper.util.rholang.Tools
import coop.rchain.models.GUnforgeable.UnfInstance.GPrivateBody
import coop.rchain.models.{GPrivate, GUnforgeable, Par}
import coop.rchain.node.encode.JsonEncoder.convertCcodecToScodec
import coop.rchain.node.web.Transaction.TransactionStore
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
  def getTransaction(blockHash: String): F[Seq[TransactionInfo]]
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
  private def findUserDeployTransaction(
      d: DeployInfoWithEventData,
      report: SingleReport,
      index: Int
  ) = {
    val transactions = findTransactions(report)
    index match {
      // the index order is based on how we play a user deploy
      case 0 =>
        transactions.map(t => TransactionInfo(t, PreCharge(d.deployInfo.get.sig))).pure
      case 1 =>
        transactions.map(t => TransactionInfo(t, UserDeploy(d.deployInfo.get.sig))).pure
      case 2 =>
        transactions.map(t => TransactionInfo(t, Refund(d.deployInfo.get.sig))).pure
      case _ =>
        Sync[F].raiseError[Vector[TransactionInfo]](
          new Exception(
            "It is not possible that user deploy got more than 3 reports(precharge, user, refund)."
          )
        )
    }
  }

  def getTransaction(blockHash: String): F[Seq[TransactionInfo]] =
    for {
      report <- blockReportAPI.blockReport(blockHash, forceReplay = false)
      ts <- report match {
             case Right(b) =>
               for {
                 udts <- b.deploys.toList.traverse(d => {
                          d.report.zipWithIndex.toList.traverse {
                            case (report, index) => findUserDeployTransaction(d, report, index)
                          }
                        })
                 userTransactions = udts.flatten.flatten
                 systemDeployTransactions = b.systemDeploys.flatMap { s =>
                   // system doesn't get precharge and refund , so it would always get one
                   val transactions = findTransactions(s.report.head)
                   s.systemDeploy.get.systemDeploy.value match {
                     case SlashSystemDeployDataProto(_, _) =>
                       transactions.map(
                         t => TransactionInfo(t, SlashingDeploy(blockHash))
                       )
                     case CloseBlockSystemDeployDataProto() =>
                       transactions.map(t => TransactionInfo(t, CloseBlock(blockHash)))
                   }
                 }
                 transactions = userTransactions ++ systemDeployTransactions
               } yield transactions

             case Left(_) => Seq.empty.pure
           }
    } yield ts

  private def findTransactions(report: SingleReport): Vector[Transaction] = {
    val transactions = report.events.foldLeft(Vector.empty[Transaction]) {
      case (acc, reportProto) =>
        reportProto.report.value match {
          case ReportCommProto(Some(consume), produces) =>
            consume.channels.headOption
              .flatMap { channel =>
                if (channel == transferUnforgeable) {
                  // based on RevVault.rho:line188 transfer method
                  // _transferTemplate!(ownRevAddress, *purse, revAddress, amount, *authKey, *ret2)
                  val produce        = produces.head
                  val fromAddr       = produce.data.get.pars.head.exprs.head.getGString
                  val toAddr         = produce.data.get.pars(2).exprs.head.getGString
                  val amount         = produce.data.get.pars(3).exprs.head.getGInt
                  val retUnforgeable = produce.data.get.pars(5)
                  val transaction    = Transaction(fromAddr, toAddr, amount, retUnforgeable, None)
                  Some(acc :+ transaction)
                } else None
              }
              .getOrElse(acc)

          case _ => acc
        }
    }

    // traverse all the transactions to check if the transaction is completed or fail with reason
    transactions.foldLeft(Vector.empty[Transaction]) {
      case (acc, transaction) =>
        val transactionWithResult = report.events
          .find { p =>
            p.report.value match {
              case ReportProduceProto(channel, _) =>
                channel
                  .flatMap { c =>
                    Some(c == transaction.retUnforgeable)
                  }
                  .getOrElse(false)
              case _ => false
            }
          }
          .flatMap { event =>
            event.report.value match {
              case ReportProduceProto(_, data) =>
                val success =
                  data.get.pars.head.exprs.head.getETupleBody.ps.head.exprs.head.getGBool
                val failReason =
                  if (success) None
                  else
                    Some(data.get.pars.head.exprs.head.getETupleBody.ps(1).exprs.head.getGString)
                Some(transaction.copy(failReason = failReason))
              case _ => None
            }
          }
          .getOrElse(transaction)
        acc :+ transactionWithResult
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
            transactions <- transactionAPI.getTransaction(blockHash)
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
    kvm.database("transaction", utf8, transactionResponseCodec) >>= { s =>
      CacheTransactionAPI(
        transactionAPI,
        s
      ).pure
    }
}
