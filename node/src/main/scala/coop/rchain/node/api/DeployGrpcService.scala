package coop.rchain.node.api

import coop.rchain.node.diagnostics
import coop.rchain.p2p.effects._
import io.grpc.{Server, ServerBuilder}

import scala.concurrent.Future
import cats._
import cats.data._
import cats.implicits._
import com.google.protobuf.empty.Empty
import coop.rchain.casper.{MultiParentCasper, PrettyPrinter, SafetyOracle}
import coop.rchain.casper.protocol._
import coop.rchain.casper.util.ProtoUtil
import coop.rchain.casper.protocol.{Deploy, DeployServiceGrpc, DeployServiceResponse, DeployString}
import coop.rchain.casper.util.rholang.InterpreterUtil
import coop.rchain.catscontrib._
import Catscontrib._
import coop.rchain.crypto.codec.Base16
import coop.rchain.node.model.repl._
import coop.rchain.node.model.diagnostics._
import coop.rchain.rholang.interpreter.{RholangCLI, Runtime}
import coop.rchain.rholang.interpreter.storage.StoragePrinter
import monix.eval.Task
import monix.execution.Scheduler
import com.google.protobuf.ByteString
import java.io.{Reader, StringReader}

import coop.rchain.casper.api.BlockAPI
import coop.rchain.node.diagnostics.{JvmMetrics, NodeMetrics, StoreMetrics}
import coop.rchain.rholang.interpreter.errors.InterpreterError
import coop.rchain.comm.transport._
import coop.rchain.comm.discovery._
import coop.rchain.shared._

private[api] class DeployGrpcService[F[_]: Monad: MultiParentCasper: Futurable: SafetyOracle]
    extends DeployServiceGrpc.DeployService {
  override def doDeploy(d: DeployString): Future[DeployServiceResponse] =
    InterpreterUtil.mkTerm(d.term) match {
      case Right(term) =>
        val deploy = Deploy(
          user = d.user,
          nonce = d.nonce,
          term = Some(term),
          sig = d.sig
        )
        val f = for {
          _ <- MultiParentCasper[F].deploy(deploy)
        } yield DeployServiceResponse(true, "Success!")

        f.toFuture

      case Left(err) =>
        Future.successful(DeployServiceResponse(false, s"Error in parsing term: \n$err"))
    }

  override def createBlock(e: Empty): Future[MaybeBlockMessage] = BlockAPI.createBlock[F].toFuture

  override def addBlock(b: BlockMessage): Future[Empty] = BlockAPI.addBlock[F](b).toFuture

  override def showBlock(q: BlockQuery): Future[BlockQueryResponse] =
    BlockAPI.getBlockQueryResponse[F](q).toFuture

  override def showBlocks(e: Empty): Future[BlocksResponse] =
    BlockAPI.getBlocksResponse[F].toFuture
}
