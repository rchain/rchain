package coop.rchain.casper.util.rholang

import cats.data.{EitherT, WriterT}
import cats.effect.Sync
import cats.syntax.all._
import com.google.protobuf.ByteString
import coop.rchain.casper.protocol.ProcessedSystemDeploy.Failed
import coop.rchain.casper.protocol.{
  Bond,
  CloseBlockSystemDeployData,
  DeployData,
  Empty,
  Event,
  ProcessedDeploy,
  ProcessedSystemDeploy,
  SlashSystemDeployData,
  SystemDeployData
}
import coop.rchain.casper.util.rholang.InterpreterUtil.printDeployErrors
import coop.rchain.casper.util.rholang.SystemDeployPlatformFailure.{
  ConsumeFailed,
  GasRefundFailure,
  UnexpectedResult,
  UnexpectedSystemErrors
}
import coop.rchain.casper.util.rholang.SystemDeployPlayResult.{PlayFailed, PlaySucceeded}
import coop.rchain.casper.util.rholang.SystemDeployUserError.SystemDeployError
import coop.rchain.casper.util.rholang.costacc.{
  CloseBlockDeploy,
  PreChargeDeploy,
  RefundDeploy,
  SlashDeploy
}
import coop.rchain.casper.util.{ConstructDeploy, EventConverter}
import coop.rchain.casper.{CasperMetricsSource, PrettyPrinter}
import coop.rchain.crypto.PublicKey
import coop.rchain.crypto.codec.Base16
import coop.rchain.crypto.signatures.{Secp256k1, Signed}
import coop.rchain.metrics.implicits._
import coop.rchain.metrics.{Metrics, Span}
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.models.Expr.ExprInstance.EVarBody
import coop.rchain.models.Validator.Validator
import coop.rchain.models.Var.VarInstance.FreeVar
import coop.rchain.models._
import coop.rchain.models.block.StateHash.StateHash
import coop.rchain.rholang.interpreter.RhoRuntime.{bootstrapRegistry, RuntimeMetricsSource}
import coop.rchain.rholang.interpreter.SystemProcesses.BlockData
import coop.rchain.rholang.interpreter.accounting.Cost
import coop.rchain.rholang.interpreter.errors.BugFoundError
import coop.rchain.rholang.interpreter.{EvaluateResult, ReplayRhoRuntime, RhoRuntime}
import coop.rchain.rspace.hashing.Blake2b256Hash
import coop.rchain.rspace.history.History.emptyRootHash
import coop.rchain.rspace.trace
import coop.rchain.rspace.util.ReplayException
import coop.rchain.shared.Log

trait RhoRuntimeSyntax {
  implicit final def syntaxRuntime[F[_]: Sync: Span: Log](
      runtime: RhoRuntime[F]
  ): RhoRuntimeOps[F] =
    new RhoRuntimeOps[F](runtime)

  implicit final def syntaxReplayRuntime[F[_]: Sync: Span: Log](
      runtime: ReplayRhoRuntime[F]
  ): ReplayRhoRuntimeOps[F] =
    new ReplayRhoRuntimeOps[F](runtime)
}

final class RhoRuntimeOps[F[_]: Sync: Span: Log](
    private val runtime: RhoRuntime[F]
) {

  implicit val RuntimeMetricsSource = Metrics.Source(CasperMetricsSource, "rho-runtime")

  private val systemDeployConsumeAllPattern = {
    import coop.rchain.models.rholang.{implicits => toPar}
    BindPattern(List(toPar(Expr(EVarBody(EVar(Var(FreeVar(0))))))), freeCount = 1)
  }

  /**
    * Because of the history legacy, the emptyStateHash does not really represent an empty trie.
    * The `emptyStateHash` is used as genesis block pre state which the state only contains registry
    * fixed channels in the state.
    */
  def emptyStateHash: F[StateHash] =
    for {
      _          <- runtime.reset(emptyRootHash)
      _          <- bootstrapRegistry(runtime)
      checkpoint <- runtime.createCheckpoint
      hash       = ByteString.copyFrom(checkpoint.root.bytes.toArray)
    } yield hash

  private def activateValidatorQuerySource: String =
    s"""
       # new return, rl(`rho:registry:lookup`), poSCh in {
       #   rl!(`rho:rchain:pos`, *poSCh) |
       #   for(@(_, PoS) <- poSCh) {
       #     @PoS!("getActiveValidators", *return)
       #   }
       # }
       """.stripMargin('#')

  private def bondsQuerySource: String =
    s"""
       # new return, rl(`rho:registry:lookup`), poSCh in {
       #   rl!(`rho:rchain:pos`, *poSCh) |
       #   for(@(_, PoS) <- poSCh) {
       #     @PoS!("getBonds", *return)
       #   }
       # }
       """.stripMargin('#')

  private def toValidatorSeq(validatorsPar: Par): Seq[Validator] =
    validatorsPar.exprs.head.getEListBody.ps.map { validator =>
      assert(validator.exprs.length == 1, "Validator in bonds map wasn't a single string.")
      validator.exprs.head.getGByteArray
    }.toList

  private def toBondSeq(bondsMap: Par): Seq[Bond] =
    bondsMap.exprs.head.getEMapBody.ps.map {
      case (validator: Par, bond: Par) =>
        assert(validator.exprs.length == 1, "Validator in bonds map wasn't a single string.")
        assert(bond.exprs.length == 1, "Stake in bonds map wasn't a single integer.")
        val validatorName = validator.exprs.head.getGByteArray
        val stakeAmount   = bond.exprs.head.getGInt
        Bond(validatorName, stakeAmount)
    }.toList

  def getActiveValidators(startHash: StateHash): F[Seq[Validator]] =
    playExploratoryDeploy(activateValidatorQuerySource, startHash)
      .ensureOr(
        validatorsPar =>
          new IllegalArgumentException(
            s"Incorrect number of results from query of current active validator in state ${PrettyPrinter
              .buildString(startHash)}: ${validatorsPar.size}"
          )
      )(validatorsPar => validatorsPar.size == 1)
      .map(validatorsPar => toValidatorSeq(validatorsPar.head))

  def computeBonds(hash: StateHash): F[Seq[Bond]] =
    // Create a deploy with newly created private key
    playExploratoryDeploy(bondsQuerySource, hash)
      .ensureOr(
        bondsPar =>
          new IllegalArgumentException(
            s"Incorrect number of results from query of current bonds in state ${PrettyPrinter
              .buildString(hash)}: ${bondsPar.size}"
          )
      )(bondsPar => bondsPar.size == 1)
      .map { bondsPar =>
        toBondSeq(bondsPar.head)
      }

  def getDataPar(channel: Par): F[Seq[Par]] =
    runtime.getData(channel).map(_.flatMap(_.a.pars))

  def getContinuationPar(channels: Seq[Par]): F[Seq[(Seq[BindPattern], Par)]] =
    runtime
      .getContinuation(channels)
      .map(
        _.filter(_.continuation.taggedCont.isParBody)
          .map(result => (result.patterns, result.continuation.taggedCont.parBody.get.body))
      )

  def captureResultsWithErrors(
      start: StateHash,
      deploy: Signed[DeployData],
      name: Par
  ): F[Seq[Par]] =
    runtime.reset(Blake2b256Hash.fromByteString(start)) >>
      evaluate(deploy)
        .flatMap({ res =>
          if (res.errors.nonEmpty) Sync[F].raiseError[EvaluateResult](res.errors.head)
          else res.pure[F]
        }) >> getDataPar(name)

  // Return channel on which result is captured is the first name
  // in the deploy term `new return in { return!(42) }`
  def captureResults(
      start: StateHash,
      deploy: Signed[DeployData]
  ): F[Seq[Par]] = {
    // Create return channel as first unforgeable name created in deploy term
    val rand = Tools.unforgeableNameRng(deploy.pk, deploy.data.timestamp)
    import coop.rchain.models.rholang.implicits._
    val returnName: Par = GPrivate(ByteString.copyFrom(rand.next()))
    captureResults(start, deploy, returnName)
  }

  def captureResults(start: StateHash, deploy: Signed[DeployData], name: Par): F[Seq[Par]] =
    captureResultsWithErrors(start, deploy, name)
      .handleErrorWith(
        ex =>
          BugFoundError(s"Unexpected error while capturing results from Rholang: $ex")
            .raiseError[F, Seq[Par]]
      )

  def playExploratoryDeploy(term: String, hash: StateHash): F[Seq[Par]] = {
    // Create a deploy with newly created private key
    val (privKey, _) = Secp256k1.newKeyPair

    // Creates signed deploy
    val deploy = ConstructDeploy.sourceDeploy(
      term,
      timestamp = System.currentTimeMillis,
      // Hardcoded phlogiston limit / 1 REV if phloPrice=1
      phloLimit = 100 * 1000 * 1000,
      sec = privKey
    )

    // Create return channel as first private name created in deploy term
    val rand = Tools.unforgeableNameRng(deploy.pk, deploy.data.timestamp)
    import coop.rchain.models.rholang.implicits._
    val returnName: Par = GPrivate(ByteString.copyFrom(rand.next()))

    // Execute deploy on top of specified block hash
    captureResults(hash, deploy, returnName)
  }

  def computeGenesis(
      terms: Seq[Signed[DeployData]],
      blockTime: Long,
      blockNumber: Long
  ): F[(StateHash, StateHash, Seq[ProcessedDeploy])] =
    Span[F].traceI("compute-genesis") {
      for {
        _ <- runtime.setBlockData(
              BlockData(blockTime, blockNumber, PublicKey(Array[Byte]()), 0)
            )
        genesisPreStateHash <- emptyStateHash
        evalResult          <- processDeploys(genesisPreStateHash, terms, processDeploy)
      } yield (genesisPreStateHash, evalResult._1, evalResult._2)
    }

  def processDeployWithCostAccounting(deploy: Signed[DeployData]) = {
    import cats.instances.vector._
    EitherT(
      WriterT(
        Log[F].info(
          s"PreCharging ${Base16.encode(deploy.pk.bytes)} for ${deploy.data.totalPhloCharge}"
        ) >>
          /* execute pre-charge */
          Span[F].traceI("precharge") {
            playSystemDeployInternal(
              new PreChargeDeploy(
                deploy.data.totalPhloCharge,
                deploy.pk,
                SystemDeployUtil.generatePreChargeDeployRandomSeed(deploy)
              )
            )
          }
      )
    ).semiflatMap( // execute user deploy
        _ =>
          WriterT(
            Span[F]
              .traceI("user-deploy") { processDeploy(deploy) }
              .map(pd => (pd.deployLog.toVector, pd))
          )
      )
      .flatTap(
        pd =>
          /*execute Refund as a side effect (keeping the logs and the possible error but discarding result)*/
          EitherT(
            WriterT(
              Log[F].info(
                s"Refunding ${Base16.encode(deploy.pk.bytes)} with ${pd.refundAmount}"
              ) >>
                Span[F].traceI("refund") {
                  playSystemDeployInternal(
                    new RefundDeploy(
                      pd.refundAmount,
                      SystemDeployUtil.generateRefundDeployRandomSeed(deploy)
                    )
                  )
                }
            )
          ).leftSemiflatMap(
            error =>
              /*If pre-charge succeeds and refund fails, it's a platform error and we should signal it with raiseError*/ WriterT
                .liftF(
                  Log[F].warn(s"Refund failure '${error.errorMsg}'") >> GasRefundFailure(
                    error.errorMsg
                  ).raiseError
                )
          )
      )
      .onError {
        case SystemDeployError(errorMsg) =>
          EitherT.right(WriterT.liftF(Log[F].warn(s"Deploy failure '$errorMsg'")))
      }
      .valueOr {
        /* we can end up here if any of the PreCharge threw an user error
        we still keep the logs (from the underlying WriterT) which we'll fill in the next step.
        We're assigning it 0 cost - replay should reach the same state
         */
        case SystemDeployError(errorMsg) =>
          ProcessedDeploy
            .empty(deploy)
            .copy(systemDeployError = Some(errorMsg))
      }
      .run // run the computation and produce the logs
      .map { case (accLog, pd) => pd.copy(deployLog = accLog.toList) }
  }

  def processDeploy(
      deploy: Signed[DeployData]
  ): F[ProcessedDeploy] = Span[F].withMarks("process-deploy") {
    for {
      fallback                     <- runtime.createSoftCheckpoint
      evaluateResult               <- evaluate(deploy)
      EvaluateResult(cost, errors) = evaluateResult
      checkpoint                   <- runtime.createSoftCheckpoint
      deployResult = ProcessedDeploy(
        deploy,
        Cost.toProto(cost),
        checkpoint.log.map(EventConverter.toCasperEvent).toList,
        errors.nonEmpty
      )
      _ <- (runtime.revertToSoftCheckpoint(fallback) <* printDeployErrors(deploy.sig, errors))
            .whenA(errors.nonEmpty)
    } yield deployResult
  }

  def consumeResult(
      channel: Par,
      pattern: BindPattern
  ): F[Option[(TaggedContinuation, Seq[ListParWithRandom])]] =
    runtime.consumeResult(Seq(channel), Seq(pattern))

  def consumeSystemResult[S <: SystemDeploy](
      systemDeploy: SystemDeploy
  ): F[Option[(TaggedContinuation, Seq[ListParWithRandom])]] =
    consumeResult(systemDeploy.returnChannel, systemDeployConsumeAllPattern)

  def playSystemDeployInternal[S <: SystemDeploy](
      systemDeploy: S
  ): F[(Vector[Event], Either[SystemDeployError, systemDeploy.Result])] =
    for {
      evaluateResult <- Span[F].traceI("evaluate-system-source") {
                         evaluateSystemSource(systemDeploy)
                       }
      maybeConsumedTuple <- if (evaluateResult.failed)
                             UnexpectedSystemErrors(evaluateResult.errors).raiseError
                           else
                             Span[F].traceI("consume-system-result") {
                               consumeSystemResult(systemDeploy)
                             }
      resultOrSystemDeployError <- maybeConsumedTuple match {
                                    case Some((_, Seq(ListParWithRandom(Seq(par), _)))) =>
                                      systemDeploy.extractResult(par) match {
                                        case Left(
                                            systemDeployPlatformFailure: SystemDeployPlatformFailure
                                            ) =>
                                          systemDeployPlatformFailure.raiseError
                                        // sorry about that but scala seems to be confused with everything else and promotes Left to SystemDeployFailure nonetheless
                                        case otherwise =>
                                          otherwise
                                            .asInstanceOf[
                                              Either[SystemDeployError, systemDeploy.Result]
                                            ]
                                            .pure
                                      }
                                    case Some((_, unexpectedResults)) =>
                                      UnexpectedResult(unexpectedResults.flatMap(_.pars)).raiseError
                                    case None => ConsumeFailed.raiseError
                                  }
      postDeploySoftCheckpoint <- runtime.createSoftCheckpoint
      log                      = postDeploySoftCheckpoint.log
    } yield (log.map(EventConverter.toCasperEvent).toVector, resultOrSystemDeployError)

  def evaluateSystemSource[S <: SystemDeploy](systemDeploy: S): F[EvaluateResult] =
    runtime.evaluate(systemDeploy.source, Cost.UNSAFE_MAX, systemDeploy.env)(
      systemDeploy.rand
    )

  def evaluate(deploy: Signed[DeployData]): F[EvaluateResult] = {
    import coop.rchain.models.rholang.implicits._
    runtime.evaluate(
      deploy.data.term,
      Cost(deploy.data.phloLimit),
      NormalizerEnv(deploy).toEnv
    )(Tools.unforgeableNameRng(deploy.pk, deploy.data.timestamp))
  }

  def processDeploys(
      startHash: StateHash,
      terms: Seq[Signed[DeployData]],
      processDeploy: Signed[DeployData] => F[ProcessedDeploy]
  ): F[(StateHash, Seq[ProcessedDeploy])] = {
    import cats.instances.list._

    for {
      _               <- runtime.reset(Blake2b256Hash.fromByteString(startHash))
      res             <- terms.toList.traverse(processDeploy)
      finalCheckpoint <- runtime.createCheckpoint
      finalStateHash  = finalCheckpoint.root
    } yield (finalStateHash.toByteString, res)
  }
  def playSystemDeploy[S <: SystemDeploy](stateHash: StateHash)(
      systemDeploy: S
  ): F[SystemDeployPlayResult[systemDeploy.Result]] =
    runtime.reset(Blake2b256Hash.fromByteString(stateHash)) >>
      playSystemDeployInternal(systemDeploy) >>= {
      case (eventLog, Right(result)) =>
        runtime.createCheckpoint.map(_.root.toByteString) >>= { finalStateHash =>
          systemDeploy match {
            case SlashDeploy(invalidBlockHash, pk, _) =>
              SystemDeployPlayResult
                .playSucceeded(
                  finalStateHash,
                  eventLog,
                  SystemDeployData.from(invalidBlockHash, pk),
                  result
                )
                .pure
            case CloseBlockDeploy(_) =>
              SystemDeployPlayResult
                .playSucceeded(
                  finalStateHash,
                  eventLog,
                  SystemDeployData.from(),
                  result
                )
                .pure
            case _ =>
              SystemDeployPlayResult
                .playSucceeded(finalStateHash, eventLog, SystemDeployData.empty, result)
                .pure
          }
        }
      case (eventLog, Left(systemDeployError)) =>
        SystemDeployPlayResult.playFailed(eventLog, systemDeployError).pure
    }

  def computeState(
      startHash: StateHash,
      terms: Seq[Signed[DeployData]],
      systemDeploys: Seq[SystemDeploy],
      blockData: BlockData,
      invalidBlocks: Map[BlockHash, Validator] = Map.empty[BlockHash, Validator]
  ): F[(StateHash, Seq[ProcessedDeploy], Seq[ProcessedSystemDeploy])] =
    Span[F].traceI("compute-state") {
      for {
        _ <- runtime.setBlockData(blockData)
        _ <- runtime.setInvalidBlocks(invalidBlocks)
        deployProcessResult <- Span[F].withMarks("process-deploys") {
                                processDeploys(
                                  startHash,
                                  terms,
                                  processDeployWithCostAccounting
                                )
                              }
        (startHash, processedDeploys) = deployProcessResult
        systemDeployProcessResult <- {
          import cats.instances.list._
          systemDeploys.toList.foldM((startHash, Vector.empty[ProcessedSystemDeploy])) {
            case ((startHash, processedSystemDeploys), sd) =>
              playSystemDeploy(startHash)(sd) >>= {
                case PlaySucceeded(stateHash, processedSystemDeploy, _) =>
                  (stateHash, processedSystemDeploys :+ processedSystemDeploy).pure[F]
                case PlayFailed(Failed(_, errorMsg)) =>
                  new Exception(
                    "Unexpected system error during play of system deploy: " + errorMsg
                  ).raiseError[F, (StateHash, Vector[ProcessedSystemDeploy])]
              }
          }
        }
        (postStateHash, processedSystemDeploys) = systemDeployProcessResult
      } yield (postStateHash, processedDeploys, processedSystemDeploys)
    }
}

final class ReplayRhoRuntimeOps[F[_]: Sync: Span: Log](
    private val runtime: ReplayRhoRuntime[F]
) {
  implicit val rhoRuntimeOps = new RhoRuntimeOps(runtime)

  case class ReplayResult[B](deploysResults: Seq[B], systemDeploysResults: Seq[B])

  def replaySystemDeployInternal[S <: SystemDeploy](
      systemDeploy: S,
      expectedFailure: Option[SystemDeployUserError]
  ): EitherT[F, ReplayFailure, Either[SystemDeployUserError, systemDeploy.Result]] =
    EitherT
      .liftF(rhoRuntimeOps.evaluateSystemSource(systemDeploy))
      .ensureOr(evaluateResult => UnexpectedSystemErrors(evaluateResult.errors))(_.succeeded)
      .semiflatMap(_ => rhoRuntimeOps.consumeSystemResult(systemDeploy))
      .subflatMap {
        case Some((_, Seq(ListParWithRandom(Seq(par), _)))) =>
          systemDeploy.extractResult(par)
        case Some((_, unexpectedResults)) =>
          UnexpectedResult(unexpectedResults.flatMap(_.pars)).asLeft
        case None => ConsumeFailed.asLeft
      }
      .leftSemiflatMap {
        case platformFailure: SystemDeployPlatformFailure =>
          platformFailure.raiseError[F, SystemDeployUserError]
        case failure: SystemDeployUserError => failure.pure
      }
      .transform {
        case Left(error) =>
          expectedFailure.fold(
            ReplayFailure
              .replayStatusMismatch(initialFailed = false, replayFailed = true)
              .asLeft[Either[SystemDeployUserError, systemDeploy.Result]]
          )(
            expected =>
              Either.cond(
                error == expected,
                Left(error),
                ReplayFailure.systemDeployErrorMismatch(expected.errorMessage, error.errorMessage)
              )
          )
        case Right(result) =>
          Either.cond(
            expectedFailure.isEmpty,
            Right(result),
            ReplayFailure.replayStatusMismatch(initialFailed = true, replayFailed = false)
          )
      }
      .flatTap(_ => EitherT.right(runtime.createSoftCheckpoint)) //We need a clear demarcation of system deploys

  def rigAndReset(stateHash: StateHash, log: trace.Log) =
    runtime.rig(log) >> runtime.reset(Blake2b256Hash.fromByteString(stateHash))

  def replaySystemDeploy[S <: SystemDeploy](stateHash: StateHash)(
      systemDeploy: S,
      processedSystemDeploy: ProcessedSystemDeploy
  ): F[Either[ReplayFailure, SystemDeployReplayResult[systemDeploy.Result]]] =
    for {
      _ <- rigAndReset(
            stateHash,
            processedSystemDeploy.eventList.map(EventConverter.toRspaceEvent)
          )
      expectedFailure = processedSystemDeploy
        .fold(_ => None, (_, errorMsg) => Some(SystemDeployError(errorMsg)))
      replayed <- replaySystemDeployInternal(systemDeploy, expectedFailure)
                   .flatMap(
                     result =>
                       runtime.checkReplayData.attemptT
                         .leftMap {
                           case replayException: ReplayException =>
                             ReplayFailure.unusedCOMMEvent(replayException)
                           case throwable => ReplayFailure.internalError(throwable)
                         }
                         .semiflatMap(
                           _ =>
                             result match {
                               case Right(value) =>
                                 runtime.createCheckpoint
                                   .map(
                                     checkpoint =>
                                       SystemDeployReplayResult
                                         .replaySucceeded(checkpoint.root.toByteString, value)
                                   )
                               case Left(failure) =>
                                 SystemDeployReplayResult
                                   .replayFailed[systemDeploy.Result](failure)
                                   .pure
                             }
                         )
                   )
                   .value
    } yield replayed

  def replayDeploy(withCostAccounting: Boolean)(
      processedDeploy: ProcessedDeploy
  ): F[Option[ReplayFailure]] =
    replayDeployE(withCostAccounting)(processedDeploy).swap.toOption.value

  def replayDeployE(withCostAccounting: Boolean)(
      processedDeploy: ProcessedDeploy
  ): EitherT[F, ReplayFailure, Unit] = {
    import processedDeploy._
    val deployEvaluator = EitherT
      .liftF {
        for {
          fallback  <- runtime.createSoftCheckpoint
          result    <- rhoRuntimeOps.evaluate(deploy)
          logErrors = printDeployErrors(processedDeploy.deploy.sig, result.errors)
          /* Since the state of `replaySpace` is reset on each invocation of `replayComputeState`,
            and `ReplayFailure`s mean that block processing is cancelled upstream, we only need to
            reset state if the replay effects of valid deploys need to be discarded. */
          _ <- (runtime.revertToSoftCheckpoint(fallback) <* logErrors).whenA(result.failed)
        } yield result
      }
      .ensureOr(
        /* Regardless of success or failure, verify that deploy status' match. */
        result => ReplayFailure.replayStatusMismatch(isFailed, result.failed)
      )(result => isFailed == result.failed)
      .ensureOr(
        result =>
          /* Verify evaluation costs match. */
          ReplayFailure.replayCostMismatch(cost.cost, result.cost.value)
      )(result => cost.cost == result.cost.value)

    def evaluatorT: EitherT[F, ReplayFailure, Boolean] =
      if (withCostAccounting) {
        val expectedFailure = processedDeploy.systemDeployError.map(SystemDeployError)
        replaySystemDeployInternal(
          new PreChargeDeploy(
            deploy.data.totalPhloCharge,
            deploy.pk,
            SystemDeployUtil.generatePreChargeDeployRandomSeed(processedDeploy.deploy)
          ),
          expectedFailure
        ).flatTap(_ => EitherT.liftF(Span[F].mark("precharge-done")))
          .flatMap { _ =>
            if (expectedFailure.isEmpty)
              deployEvaluator
                .semiflatMap(
                  evalResult =>
                    for {
                      _ <- Span[F].mark("deploy-eval-done")
                      r <- runtime.createSoftCheckpoint
                            .whenA(evalResult.succeeded)
                            .as(evalResult.succeeded)
                      _ <- Span[F].mark("deploy-done")
                    } yield r
                )
                .flatTap(_ => EitherT.liftF(Span[F].mark("refund-started")))
                .flatMap { succeeded =>
                  replaySystemDeployInternal(
                    new RefundDeploy(
                      refundAmount,
                      SystemDeployUtil.generateRefundDeployRandomSeed(processedDeploy.deploy)
                    ),
                    None
                  ).map {
                    case Left(_)  => false
                    case Right(_) => true
                  }
                }
                .flatTap(_ => EitherT.liftF(Span[F].mark("refund-done")))
            else EitherT.rightT(true)
          }
      } else deployEvaluator.map(_.succeeded)

    for {
      _ <- EitherT.liftF(runtime.rig(processedDeploy.deployLog.map(EventConverter.toRspaceEvent)))
      failureOption <- evaluatorT
                        .flatMap { succeeded =>
                          /* This deployment represents either correct program `Some(result)`,
or we have a failed pre-charge (`None`) but we agree on that it failed.
In both cases we want to check reply data and see if everything is in order */
                          runtime.checkReplayData.attemptT
                            .leftMap {
                              case replayException: ReplayException =>
                                ReplayFailure.unusedCOMMEvent(replayException)
                              case throwable => ReplayFailure.internalError(throwable)
                            }
                            .leftFlatMap {
                              case UnusedCOMMEvent(_) if !succeeded =>
                                // TODO: temp fix for replay error mismatch
                                // https://rchain.atlassian.net/browse/RCHAIN-3505
                                EitherT.rightT[F, ReplayFailure](())
                              case ex: ReplayFailure => EitherT.leftT[F, Unit](ex)
                            }
                        }
    } yield failureOption
  }

  def replayDeploys(
      startHash: StateHash,
      terms: Seq[ProcessedDeploy],
      systemDeploys: Seq[ProcessedSystemDeploy],
      replayDeploy: ProcessedDeploy => F[Option[ReplayFailure]],
      replaySystemDeploy: ProcessedSystemDeploy => F[Option[ReplayFailure]]
  ): F[Either[ReplayFailure, StateHash]] =
    (for {
      _ <- EitherT.right(runtime.reset(Blake2b256Hash.fromByteString(startHash)))
      _ <- EitherT(
            terms.tailRecM { ts =>
              if (ts.isEmpty)
                ().asRight[ReplayFailure].asRight[Seq[ProcessedDeploy]].pure[F]
              else
                Span[F].traceI("replay-deploy") {
                  replayDeploy(ts.head).map(_.map(_.asLeft[Unit]).toRight(ts.tail))
                }
            }
          )
      _ <- EitherT(
            systemDeploys.tailRecM { ts =>
              if (ts.isEmpty)
                ().asRight[ReplayFailure].asRight[Seq[ProcessedSystemDeploy]].pure[F]
              else
                Span[F].traceI("replay-sys-deploy") {
                  replaySystemDeploy(ts.head).map(_.map(_.asLeft[Unit]).toRight(ts.tail))
                }
            }
          )
      res <- EitherT.right[ReplayFailure](Span[F].traceI("create-checkpoint") {
              runtime.createCheckpoint
            })
    } yield res.root.toByteString).value

  def replayComputeState(startHash: StateHash)(
      terms: Seq[ProcessedDeploy],
      systemDeploys: Seq[ProcessedSystemDeploy],
      blockData: BlockData,
      invalidBlocks: Map[BlockHash, Validator] = Map.empty[BlockHash, Validator],
      isGenesis: Boolean //FIXME have a better way of knowing this. Pass the replayDeploy function maybe?
  ): F[Either[ReplayFailure, StateHash]] =
    Span[F].traceI("replay-compute-state") {
      for {
        _ <- runtime.setBlockData(blockData)
        _ <- runtime.setInvalidBlocks(invalidBlocks)
        result <- replayDeploys(
                   startHash,
                   terms,
                   systemDeploys,
                   replayDeploy(withCostAccounting = !isGenesis),
                   replaySystemDeploy(blockData)
                 )
      } yield result
    }

  def replaySystemDeployInternalE(
      systemDeploy: SystemDeploy,
      processedSystemDeploy: ProcessedSystemDeploy
  ): EitherT[F, ReplayFailure, Unit] = {
    val deployEvaluator = EitherT
      .liftF {
        for {
          _ <- runtime.rig(
                processedSystemDeploy.eventList.map(EventConverter.toRspaceEvent)
              )
          fallback <- runtime.createSoftCheckpoint
          result   <- rhoRuntimeOps.evaluateSystemSource(systemDeploy)
          _        <- rhoRuntimeOps.consumeSystemResult(systemDeploy)
          /* Since the state of `replaySpace` is reset on each invocation of `replayComputeState`,
            and `ReplayFailure`s mean that block processing is cancelled upstream, we only need to
            reset state if the replay effects of valid deploys need to be discarded. */
          _ <- runtime.revertToSoftCheckpoint(fallback).whenA(result.failed)
        } yield result
      }
      .ensureOr(
        /* Regardless of success or failure, verify that deploy status' match. */
        result => ReplayFailure.replayStatusMismatch(processedSystemDeploy.failed, result.failed)
      )(result => processedSystemDeploy.failed == result.failed)

    deployEvaluator
      .semiflatMap { evalResult =>
        runtime.createSoftCheckpoint
          .whenA(evalResult.succeeded)
      }
      .flatMap { _ =>
        /* This deployment represents either correct program `Some(result)`,
              or we have a failed pre-charge (`None`) but we agree on that it failed.
              In both cases we want to check reply data and see if everything is in order */
        runtime.checkReplayData.attemptT.leftMap {
          case replayException: ReplayException =>
            ReplayFailure.unusedCOMMEvent(replayException)
          case throwable => ReplayFailure.internalError(throwable)
        }
      }
  }

  def replaySystemDeploy(
      blockData: BlockData
  )(processedSystemDeploy: ProcessedSystemDeploy): F[Option[ReplayFailure]] =
    Span[F].withMarks("replay-system-deploy")(
      replaySystemDeployE(blockData)(processedSystemDeploy).swap.toOption.value
    )

  def replaySystemDeployE(
      blockData: BlockData
  )(processedSystemDeploy: ProcessedSystemDeploy): EitherT[F, ReplayFailure, Unit] = {
    import processedSystemDeploy._
    val sender = ByteString.copyFrom(blockData.sender.bytes)
    systemDeploy match {
      case SlashSystemDeployData(invalidBlockHash, issuerPublicKey) =>
        val slashDeploy =
          SlashDeploy(
            invalidBlockHash,
            issuerPublicKey,
            SystemDeployUtil.generateSlashDeployRandomSeed(sender, blockData.seqNum)
          )
        replaySystemDeployInternalE(slashDeploy, processedSystemDeploy)
      case CloseBlockSystemDeployData =>
        val closeBlockDeploy = CloseBlockDeploy(
          SystemDeployUtil.generateCloseDeployRandomSeed(sender, blockData.seqNum)
        )
        replaySystemDeployInternalE(closeBlockDeploy, processedSystemDeploy)
      case Empty =>
        EitherT.leftT(ReplayFailure.internalError(new Exception("Expected system deploy")))
    }
  }
}
