package coop.rchain.casper.util.rholang

import cats.effect._
import cats.effect.concurrent.MVar
import cats.implicits._
import com.google.protobuf.ByteString
import coop.rchain.casper.protocol._
import coop.rchain.casper.util.ProtoUtil
import coop.rchain.casper.util.rholang.RuntimeManager.StateHash
import coop.rchain.catscontrib._
import coop.rchain.crypto.hash.Blake2b512Random
import coop.rchain.models.Expr.ExprInstance.GString
import coop.rchain.models._
import coop.rchain.rholang.interpreter.accounting.{Cost, CostAccount}
import coop.rchain.rholang.interpreter.storage.StoragePrinter
import coop.rchain.rholang.interpreter.{accounting, ChargingReducer, ErrorLog, Runtime}
import coop.rchain.rspace.internal.Datum
import coop.rchain.rspace.{Blake2b256Hash, ReplayException}
import monix.eval.Task
import monix.execution.Scheduler

import scala.collection.immutable

class RuntimeManager private (emptyStateHash: ByteString, runtimeContainer: MVar[Task, Runtime])
    extends AbstractRuntimeManager[Task](emptyStateHash, runtimeContainer)

//runtime is a SyncVar for thread-safety, as all checkpoints share the same "hot store"
class AbstractRuntimeManager[F[_]: Concurrent: ToAbstractContext] protected (
    val emptyStateHash: ByteString,
    runtimeContainer: MVar[F, Runtime]
) {

  def captureResults(start: StateHash, deploy: Deploy, name: String = "__SCALA__"): F[Seq[Par]] =
    captureResults(start, deploy, Par().withExprs(Seq(Expr(GString(name)))))

  def captureResults(start: StateHash, deploy: Deploy, name: Par): F[Seq[Par]] =
    Sync[F]
      .bracket(runtimeContainer.take) { runtime =>
        //TODO: Is better error handling needed here?
        for {
          evalR                     <- newEval(deploy :: Nil, runtime, start)
          (_, Seq(processedDeploy)) = evalR
          result <- if (processedDeploy.status.isFailed) Seq.empty[Datum[ListParWithRandom]].pure[F]
                   else {
                     val r: F[Seq[Datum[ListParWithRandom]]] =
                       ToAbstractContext[F].fromTask(runtime.space.getData(name))
                     r
                   }
        } yield result.flatMap(_.a.pars)
      }(runtime => runtimeContainer.put(runtime))

  def replayComputeState(
      hash: StateHash,
      terms: Seq[InternalProcessedDeploy],
      time: Option[Long] = None
  ): F[Either[(Option[Deploy], Failed), StateHash]] =
    Sync[F].bracket(runtimeContainer.take) { runtime =>
      for {
        _      <- setTimestamp(time, runtime)
        result <- replayEval(terms, runtime, hash)
      } yield result
    }(runtime => runtimeContainer.put(runtime))

  def computeState(
      hash: StateHash,
      terms: Seq[Deploy],
      time: Option[Long] = None
  ): F[(StateHash, Seq[InternalProcessedDeploy])] =
    Sync[F].bracket(runtimeContainer.take) { runtime =>
      for {
        _      <- setTimestamp(time, runtime)
        result <- newEval(terms, runtime, hash)
      } yield result
    }(runtime => runtimeContainer.put(runtime))

  private def setTimestamp(
      time: Option[Long],
      runtime: Runtime
  ): F[Unit] =
    time match {
      case Some(t) =>
        val timestamp: Par = Par(exprs = Seq(Expr(Expr.ExprInstance.GInt(t))))
        ToAbstractContext[F].fromTask(runtime.blockTime.setParams(timestamp))
      case None => ().pure[F]
    }

  def storageRepr(hash: StateHash): F[Option[String]] =
    Sync[F]
      .bracket(runtimeContainer.take) { runtime =>
        val blakeHash = Blake2b256Hash.fromByteArray(hash.toByteArray)
        ToAbstractContext[F].fromTask(
          runtime.space.reset(blakeHash).map(_ => StoragePrinter.prettyPrint(runtime.space.store))
        )
      }(runtime => runtimeContainer.put(runtime))
      .attempt
      .map {
        case Right(print) => Some(print)
        case Left(_)      => None
      }

  def computeBonds(hash: StateHash): F[Seq[Bond]] = {
    val bondsQuery =
      """new rl(`rho:registry:lookup`), SystemInstancesCh, posCh in {
        |  rl!(`rho:id:wdwc36f4ixa6xacck3ddepmgueum7zueuczgthcqp6771kdu8jogm8`, *SystemInstancesCh) |
        |  for(@(_, SystemInstancesRegistry) <- SystemInstancesCh) {
        |    @SystemInstancesRegistry!("lookup", "pos", *posCh) |
        |    for(pos <- posCh){ pos!("getBonds", "__SCALA__") }
        |  }
        |}""".stripMargin

    val bondsQueryTerm =
      ProtoUtil.deployDataToDeploy(ProtoUtil.sourceDeploy(bondsQuery, 0L, accounting.MAX_VALUE))
    captureResults(hash, bondsQueryTerm)
      .ensureOr(
        bondsPar =>
          new IllegalArgumentException(
            s"Incorrect number of results from query of current bonds: ${bondsPar.size}"
          )
      )(bondsPar => bondsPar.size == 1)
      .map { bondsPar =>
        toBondSeq(bondsPar.head)
      }
  }

  private def withResetRuntime[R](hash: StateHash, block: Runtime => F[R]) =
    Sync[F].bracket(runtimeContainer.take) { runtime =>
      val blakeHash = Blake2b256Hash.fromByteArray(hash.toByteArray)
      ToAbstractContext[F].fromTask(runtime.space.reset(blakeHash)).flatMap(_ => block(runtime))
    }(runtime => runtimeContainer.put(runtime))

  private def toBondSeq(bondsMap: Par): Seq[Bond] =
    bondsMap.exprs.head.getEMapBody.ps.map {
      case (validator: Par, bond: Par) =>
        assert(validator.exprs.length == 1, "Validator in bonds map wasn't a single string.")
        assert(bond.exprs.length == 1, "Stake in bonds map wasn't a single integer.")
        val validatorName = validator.exprs.head.getGByteArray
        val stakeAmount   = bond.exprs.head.getETupleBody.ps.head.exprs.head.getGInt
        Bond(validatorName, stakeAmount)
    }.toList

  def getData(hash: ByteString, channel: Par): F[Seq[Par]] =
    withResetRuntime(hash, runtime => {
      ToAbstractContext[F].fromTask(runtime.space.getData(channel).map(_.flatMap(_.a.pars)))
    })

  def getContinuation(
      hash: ByteString,
      channels: immutable.Seq[Par]
  ): F[Seq[(Seq[BindPattern], Par)]] =
    withResetRuntime(
      hash,
      runtime => {
        ToAbstractContext[F]
          .fromTask(runtime.space.getWaitingContinuations(channels))
          .map(
            results =>
              for {
                result <- results.filter(_.continuation.taggedCont.isParBody)
              } yield (result.patterns, result.continuation.taggedCont.parBody.get.body)
          )
      }
    )

  private def newEval(
      terms: Seq[Deploy],
      runtime: Runtime,
      initHash: StateHash
  ): F[(StateHash, Seq[InternalProcessedDeploy])] = {

    def doEval(
        terms: Seq[Deploy],
        hash: Blake2b256Hash,
        acc: Seq[InternalProcessedDeploy]
    ): F[(StateHash, Seq[InternalProcessedDeploy])] =
      Concurrent[F].defer {
        terms match {
          case deploy +: rem =>
            for {
              _              <- ToAbstractContext[F].fromTask(runtime.space.reset(hash))
              availablePhlos = Cost(deploy.raw.map(_.phloLimit).get)
              _              <- ToAbstractContext[F].fromTask(runtime.reducer.setAvailablePhlos(availablePhlos))
              (codeHash, phloPrice, userId, timestamp) = ProtoUtil.getRholangDeployParams(
                deploy.raw.get
              )
              _ <- ToAbstractContext[F].fromTask(
                    runtime.shortLeashParams.setParams(codeHash, phloPrice, userId, timestamp)
                  )
              injResult           <- injAttempt(deploy, runtime.reducer, runtime.errorLog)
              (phlosLeft, errors) = injResult
              cost                = phlosLeft.copy(cost = availablePhlos.value - phlosLeft.cost)
              newCheckpoint       <- ToAbstractContext[F].fromTask(runtime.space.createCheckpoint())
              deployResult = InternalProcessedDeploy(
                deploy,
                cost,
                newCheckpoint.log,
                DeployStatus.fromErrors(errors)
              )
              cont <- if (errors.isEmpty)
                       doEval(rem, newCheckpoint.root, acc :+ deployResult)
                     else doEval(rem, hash, acc :+ deployResult)
            } yield cont

          case _ => (ByteString.copyFrom(hash.bytes.toArray), acc).pure[F]
        }
      }

    doEval(terms, Blake2b256Hash.fromByteArray(initHash.toByteArray), Vector.empty)
  }

  private def replayEval(
      terms: Seq[InternalProcessedDeploy],
      runtime: Runtime,
      initHash: StateHash
  ): F[Either[(Option[Deploy], Failed), StateHash]] = {

    def doReplayEval(
        terms: Seq[InternalProcessedDeploy],
        hash: Blake2b256Hash
    ): F[Either[(Option[Deploy], Failed), StateHash]] =
      Concurrent[F].defer {
        terms match {
          case InternalProcessedDeploy(deploy, _, log, status) +: rem =>
            val availablePhlos = Cost(deploy.raw.map(_.phloLimit).get)
            for {
              _ <- ToAbstractContext[F].fromTask(
                    runtime.replayReducer.setAvailablePhlos(availablePhlos)
                  )
              (codeHash, phloPrice, userId, timestamp) = ProtoUtil.getRholangDeployParams(
                deploy.raw.get
              )
              _ <- ToAbstractContext[F].fromTask(
                    runtime.shortLeashParams.setParams(codeHash, phloPrice, userId, timestamp)
                  )
              _         <- ToAbstractContext[F].fromTask(runtime.replaySpace.rig(hash, log.toList))
              injResult <- injAttempt(deploy, runtime.replayReducer, runtime.errorLog)
              //TODO: compare replay deploy cost to given deploy cost
              (phlosLeft, errors) = injResult
              cost                = phlosLeft.copy(cost = availablePhlos.value - phlosLeft.cost)
              cont <- DeployStatus.fromErrors(errors) match {
                       case int: InternalErrors => Left(Some(deploy) -> int).pure[F]
                       case replayStatus =>
                         if (status.isFailed != replayStatus.isFailed)
                           Left(Some(deploy) -> ReplayStatusMismatch(replayStatus, status)).pure[F]
                         else if (errors.nonEmpty) doReplayEval(rem, hash)
                         else {
                           ToAbstractContext[F]
                             .fromTask(runtime.replaySpace.createCheckpoint())
                             .attempt
                             .flatMap {
                               case Right(newCheckpoint) =>
                                 doReplayEval(rem, newCheckpoint.root)
                               case Left(ex: ReplayException) =>
                                 Either
                                   .left[(Option[Deploy], Failed), StateHash](
                                     none[Deploy] -> UnusedCommEvent(ex)
                                   )
                                   .pure[F]
                             }
                         }
                     }
            } yield cont

          case _ =>
            Either
              .right[(Option[Deploy], Failed), StateHash](ByteString.copyFrom(hash.bytes.toArray))
              .pure[F]
        }
      }

    doReplayEval(terms, Blake2b256Hash.fromByteArray(initHash.toByteArray))
  }

  private def injAttempt(
      deploy: Deploy,
      reducer: ChargingReducer[Task],
      errorLog: ErrorLog
  ): F[(PCost, Vector[Throwable])] = {
    implicit val rand: Blake2b512Random = Blake2b512Random(
      DeployData.toByteArray(ProtoUtil.stripDeployData(deploy.raw.get))
    )
    ToAbstractContext[F].fromTask(
      reducer
        .inj(deploy.term.get)
        .attempt
        .flatMap(result => {
          val oldErrors = errorLog.readAndClearErrorVector()
          val newErrors = result.swap.toSeq.toVector
          val allErrors = oldErrors |+| newErrors

          reducer.getAvailablePhlos().map(phlos => CostAccount.toProto(phlos) -> allErrors)
        })
    )
  }
}

object RuntimeManager {
  type StateHash = ByteString
  import coop.rchain.catscontrib.TaskContrib._

  def fromRuntime(active: Runtime)(implicit scheduler: Scheduler): RuntimeManager =
    (for {
      _                <- active.space.clear()
      _                <- active.replaySpace.clear()
      _                <- Runtime.injectEmptyRegistryRoot(active.space, active.replaySpace)
      checkpoint       <- active.space.createCheckpoint()
      replayCheckpoint <- active.replaySpace.createCheckpoint()
      hash             = ByteString.copyFrom(checkpoint.root.bytes.toArray)
      replayHash       = ByteString.copyFrom(replayCheckpoint.root.bytes.toArray)
      _                = assert(hash == replayHash)
      runtime          <- MVar[Task].of(active)
    } yield (new RuntimeManager(hash, runtime))).unsafeRunSync
}
