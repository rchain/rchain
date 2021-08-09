package coop.rchain.node.benchmark.utils

import cats.effect.Concurrent
import cats.syntax.all._
import com.google.protobuf.ByteString
import coop.rchain.casper.protocol.{DeployData, ProcessedDeploy, ProcessedSystemDeploy}
import coop.rchain.casper.util.rholang.costacc.CloseBlockDeploy
import coop.rchain.casper.util.rholang.{RuntimeManager, SystemDeployUtil}
import coop.rchain.crypto.codec.Base16
import coop.rchain.crypto.signatures.Signed
import coop.rchain.crypto.{PrivateKey, PublicKey}
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.models.Validator.Validator
import coop.rchain.models.block.StateHash.StateHash
import coop.rchain.node.benchmark.utils
import coop.rchain.rholang.interpreter.SystemProcesses.BlockData
import coop.rchain.rholang.interpreter.util.RevAddress
import coop.rchain.shared.Time

import scala.collection.Seq

object StateTransition {

  final case class StateTransitionResult(
      finalState: StateHash,
      paymentsDone: Seq[Charged[PaymentDeploy]],
      processedDeploys: Seq[ProcessedDeploy],
      processedSysDeploys: Seq[ProcessedSystemDeploy]
  )

  /** Make state transition */
  def make[F[_]: Concurrent: RuntimeManager: Time](
      baseState: StateHash,
      validator: PublicKey,
      seqNum: Int,
      blockNum: Long,
      payments: List[Payment]
  ): F[StateTransitionResult] = {
    val perValidatorVault =
      User(PrivateKey(ByteString.EMPTY), validator, Base16.encode(validator.bytes))

    def computeState(
        userDeploys: Seq[Signed[DeployData]]
    ): F[(StateHash, Seq[ProcessedDeploy], Seq[ProcessedSystemDeploy])] = {
      val cbRandomSeed = SystemDeployUtil.generateCloseDeployRandomSeed(validator, seqNum)
      assert(userDeploys.nonEmpty, "Attempting to compute state without user deploys.")
      RuntimeManager[F]
        .computeState(baseState)(
          terms = userDeploys.distinct,
          systemDeploys = CloseBlockDeploy(cbRandomSeed) :: Nil,
          blockData = BlockData(userDeploys.head.data.timestamp, blockNum, validator, seqNum),
          invalidBlocks = Map.empty[BlockHash, Validator]
        )
    }
    for {
      paymentDeploys <- payments.traverse(Payment.mkTxDeploy(_, printDebug = false))
      r <- computeState(paymentDeploys.map(_.d)).map {
            case (s, processedDeploys, sp) =>
              assert(
                !processedDeploys.exists(_.isFailed),
                "Failed deploys found. Check if you users have enough REV to continue payments."
              )
              val charged =
                processedDeploys.map { d =>
                  val payerAddr = RevAddress.fromPublicKey(d.deploy.pk).get.address.toBase58
                  val charge = utils.Payment(
                    // TODO key not avail here, but not needed actually
                    User(PrivateKey(ByteString.EMPTY), d.deploy.pk, payerAddr),
                    perValidatorVault,
                    d.cost.cost
                  )
                  Charged(paymentDeploys.find(_.d.sig == d.deploy.sig).get, charge)
                }
              StateTransitionResult(s, charged, processedDeploys, sp)
          }
    } yield r
  }
}
