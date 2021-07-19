package com.revdefine.node.setup

import cats.effect.{ConcurrentEffect, ContextShift, ExitCode, Timer}
import com.revdefine.node.setup.UpdateFinalized.updateFinalized
import com.revdefine.node.setup.UpdateTransaction.updateTransaction
import com.revdefine.node.store.MongoStore
import com.revdefine.node.web.node.api.createAPI
import com.revdefine.node.web.initService
import coop.rchain.blockstorage.BlockStore
import coop.rchain.blockstorage.dag.BlockDagStorage
import coop.rchain.casper.SafetyOracle
import coop.rchain.casper.engine.EngineCell.EngineCell
import coop.rchain.node.web.{CacheTransactionAPI, TransactionAPI}
import coop.rchain.shared.Log
import monix.execution.Scheduler

import java.nio.file.Path

object Setup {

  final case class RevDefineServices[F[_]: ConcurrentEffect](
      httpService: fs2.Stream[F, ExitCode],
      updateTransaction: F[Unit],
      updateFinalized: F[Unit]
  ) {
    def toRunningStream =
      fs2
        .Stream(
          httpService,
          fs2.Stream
            .repeatEval(updateTransaction),
          fs2.Stream
            .repeatEval(updateFinalized)
        )
        .parJoinUnbounded
  }

  def createService[F[_]: ConcurrentEffect: Timer: Log: ContextShift: EngineCell: BlockStore: SafetyOracle: BlockDagStorage](
      dataDir: Path,
      mongo: MongoStore[F],
      transactionAPI: CacheTransactionAPI[F]
  )(implicit scheduler: Scheduler): RevDefineServices[F] =
    RevDefineServices(
      createHttp[F](dataDir, mongo),
      updateTransaction[F](mongo, transactionAPI),
      updateFinalized[F](mongo)
    )

  def createHttp[F[_]: ConcurrentEffect: Timer: Log: ContextShift: EngineCell: BlockStore: SafetyOracle](
      dataDir: Path,
      mongoStore: MongoStore[F]
  )(implicit scheduler: Scheduler): fs2.Stream[F, ExitCode] =
    fs2.Stream.eval(createAPI[F](dataDir)).flatMap(api => initService(mongoStore, api))

}
