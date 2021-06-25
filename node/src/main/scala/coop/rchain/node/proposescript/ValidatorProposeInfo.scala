package coop.rchain.node.proposescript

import cats.effect.Sync
import cats.syntax.all._
import coop.rchain.casper.protocol.{
  BlockInfo,
  BlockQuery,
  BlocksQuery,
  DeployData,
  LightBlockInfo,
  ProposeQuery,
  ProposeResultQuery
}
import coop.rchain.casper.protocol.deploy.v1.DeployServiceV1GrpcMonix
import coop.rchain.casper.protocol.propose.v1.ProposeServiceV1GrpcMonix
import coop.rchain.crypto.PrivateKey
import coop.rchain.crypto.codec.Base16
import coop.rchain.crypto.signatures.{Secp256k1, Signed}
import coop.rchain.monix.Monixable
import coop.rchain.shared.Log
import io.grpc.{ManagedChannel, ManagedChannelBuilder}
import monix.eval.Task
import coop.rchain.shared.syntax._

final case class ProposeInfo(
    deployContract: String,
    phloLimit: Long,
    phloPrice: Long,
    deployPrivateKey: String
)
final case class ValidatorProposeInfo(
    host: String,
    grpcPort: Int,
    httpPort: Int,
    proposeInfo: ProposeInfo
) {
  private val ProposeSuccessRegex      = "Success! Block ([0-9a-f]+) created and added.".r
  private val ProposeAsyncSuccessRegex = "Propose started \\(seqNum ([0-9]+)\\)".r

  private val channel: ManagedChannel =
    ManagedChannelBuilder
      .forAddress(host, grpcPort)
      .usePlaintext()
      .build

  private def deployStub  = DeployServiceV1GrpcMonix.stub(channel)
  private def proposeStub = ProposeServiceV1GrpcMonix.stub(channel)

  def getLatestBlocks[F[_]: Sync: Monixable](depth: Int = 1): F[List[LightBlockInfo]] =
    deployStub
      .getBlocks(BlocksQuery(depth = depth))
      .mapEval(
        resp =>
          Task
            .raiseError(
              new Exception(s"Getting latest block from $host:$grpcPort failed with $resp.")
            )
            .whenA(resp.message.isError || resp.message.blockInfo.isEmpty)
            .flatMap(_ => Task.pure(resp.message.blockInfo.get))
      )
      .toListL
      .fromTask

  def getBlock[F[_]: Sync: Monixable](blockHash: String): F[BlockInfo] =
    deployStub
      .getBlock(BlockQuery(hash = blockHash))
      .fromTask
      .flatTap(
        blockResp =>
          Sync[F]
            .raiseError(
              new Exception(
                s"Getting block $blockHash from $host:$grpcPort failed with ${blockResp.message.error}."
              )
            )
            .whenA(blockResp.message.isError || blockResp.message.blockInfo.isEmpty)
      )
      .map(_.message.blockInfo.get)

  def deploy[F[_]: Sync: Monixable](validAfterBlockNumber: Long): F[String] = {
    val timestamp = System.currentTimeMillis()
    val d = DeployData(
      term = proposeInfo.deployContract,
      timestamp = timestamp,
      phloPrice = proposeInfo.phloPrice,
      phloLimit = proposeInfo.phloLimit,
      validAfterBlockNumber = validAfterBlockNumber
    )
    val signedDeploy = Signed(
      d,
      Secp256k1,
      PrivateKey(Base16.decode(proposeInfo.deployPrivateKey).get)
    )
    deployStub
      .doDeploy(DeployData.toProto(signedDeploy))
      .fromTask
      .flatTap(
        resp =>
          Sync[F]
            .raiseError(
              new Exception(
                s"Deploy on $host:$grpcPort failed on ${resp.message.error} with $signedDeploy"
              )
            )
            .whenA(resp.message.isError || resp.message.result.isEmpty)
      )
      .map(_.message.result.get)
  }

  def propose[F[_]: Sync: Monixable](isAsync: Boolean): F[String] =
    proposeStub
      .propose(ProposeQuery(isAsync = isAsync))
      .fromTask
      .flatTap(
        resp =>
          Sync[F]
            .raiseError(
              new Exception(
                s"Propose on $host:$grpcPort failed on ${resp.message.error} with isAsync = $isAsync"
              )
            )
            .whenA(resp.message.isError || resp.message.result.isEmpty)
      )
      .map(_.message.result.get)

  def proposeResult[F[_]: Sync: Monixable]: F[String] =
    proposeStub
      .proposeResult(ProposeResultQuery())
      .fromTask
      .flatTap(
        proposeResResp =>
          Sync[F]
            .raiseError(
              new Exception(
                s"Getting propose result on $host:$grpcPort failed on ${proposeResResp.message.error}"
              )
            )
            .whenA(proposeResResp.message.isError || proposeResResp.message.result.isEmpty)
      )
      .map(_.message.result.get)

  def isContainBlock[F[_]: Sync: Monixable](blockHash: String): F[Boolean] =
    getBlock(blockHash = blockHash).redeem(_ => false, _ => true)

  private def deployAndPropose[F[_]: Sync: Monixable](isAsync: Boolean): F[String] =
    getLatestBlocks()
      .map(_.head)
      .flatMap(latestBlock => deploy(latestBlock.blockNumber))
      .flatMap(_ => propose(isAsync = isAsync))

  def deployAndProposeAsync[F[_]: Sync: Monixable]: F[String] =
    deployAndPropose(isAsync = true)
      .flatTap {
        case ProposeAsyncSuccessRegex(seqNum) => seqNum.pure
        case failedResult =>
          Sync[F].raiseError[String](
            new Exception(s"Propose async failed with ${failedResult} on $host: $grpcPort")
          )
      }
      .flatMap(_ => proposeResult)
      .flatMap {
        case ProposeSuccessRegex(blockHash) => blockHash.pure
        case r =>
          Sync[F].raiseError[String](
            new Exception(
              s"Getting async propose result failed with ${r} on $host: $grpcPort"
            )
          )
      }

  def deployAndProposeSync[F[_]: Sync: Monixable]: F[String] =
    deployAndPropose(isAsync = false).flatMap {
      case ProposeSuccessRegex(blockHash) => blockHash.pure
      case proposeResult =>
        Sync[F].raiseError[String](
          new Exception(
            s"Parsing propose result failed $proposeResult on $host:$grpcPort"
          )
        )
    }

  def logValidatorGotBlock[F[_]: Log](blockHash: String): F[Unit] =
    Log[F].info(s"Validator ${host}:${grpcPort} got $blockHash.")

  def logValidatorMissedBlock[F[_]: Log](blockHash: String): F[Unit] =
    Log[F].info(s"Validator ${host}:${grpcPort} hasn't got $blockHash.")

  def logValidatorProposedBlock[F[_]: Log](blockHash: String): F[Unit] =
    Log[F].info(
      s"Validator ${host}:${grpcPort} successfully proposed $blockHash."
    )
}
