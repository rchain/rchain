package coop.rchain.casper.rholang.syntax

import cats.{Applicative, Functor, Monad}
import cats.data.{EitherT, OptionT}
import cats.effect.Sync
import cats.effect.concurrent.Ref
import cats.syntax.all._
import com.google.protobuf.ByteString
import coop.rchain.casper.genesis.Genesis
import coop.rchain.casper.protocol.ProcessedSystemDeploy.Failed
import coop.rchain.casper.protocol.{Bond, DeployData, Event, ProcessedDeploy, SystemDeployData}
import coop.rchain.casper.rholang.InterpreterUtil.printDeployErrors
import coop.rchain.casper.rholang._
import coop.rchain.casper.rholang.syntax.RuntimeSyntax._
import coop.rchain.casper.rholang.sysdeploys.{
  CloseBlockDeploy,
  PreChargeDeploy,
  RefundDeploy,
  SlashDeploy
}
import coop.rchain.casper.rholang.types.SystemDeployPlatformFailure.{
  ConsumeFailed,
  GasRefundFailure,
  UnexpectedResult,
  UnexpectedSystemErrors
}
import coop.rchain.casper.rholang.types._
import coop.rchain.casper.util.{ConstructDeploy, EventConverter}
import coop.rchain.casper.{BlockRandomSeed, CasperMetricsSource, PrettyPrinter}
import coop.rchain.crypto.PublicKey
import coop.rchain.crypto.signatures.{Secp256k1, Signed}
import coop.rchain.metrics.implicits._
import coop.rchain.metrics.{Metrics, Span}
import coop.rchain.models.Expr.ExprInstance.EVarBody
import coop.rchain.models.Validator.Validator
import coop.rchain.models.Var.VarInstance.FreeVar
import coop.rchain.models._
import coop.rchain.models.block.StateHash.StateHash
import coop.rchain.models.syntax.modelsSyntaxByteString
import coop.rchain.rholang.interpreter.RhoRuntime.bootstrapRegistry
import coop.rchain.rholang.interpreter.SystemProcesses.BlockData
import coop.rchain.rholang.interpreter.accounting.Cost
import coop.rchain.rholang.interpreter.errors.BugFoundError
import coop.rchain.rholang.interpreter.merging.RholangMergingLogic
import coop.rchain.rholang.interpreter.{storage, EvaluateResult, RhoRuntime}
import coop.rchain.rspace.hashing.{Blake2b256Hash, StableHashProvider}
import coop.rchain.rspace.history.History.emptyRootHash
import coop.rchain.rspace.merger.EventLogMergingLogic.NumberChannelsEndVal
import coop.rchain.shared.{Base16, Log}
import coop.rchain.casper.rholang.RuntimeDeployResult._
import coop.rchain.crypto.hash.Blake2b512Random

trait RuntimeSyntax {
  implicit final def casperSyntaxRholangRuntime[F[_]](
      runtime: RhoRuntime[F]
  ): RuntimeOps[F] = new RuntimeOps[F](runtime)
}

object RuntimeSyntax {
  type SysEvalResult[S <: SystemDeploy] = (Either[SystemDeployUserError, S#Result], EvaluateResult)

  implicit val RuntimeMetricsSource = Metrics.Source(CasperMetricsSource, "rho-runtime")

  val systemDeployConsumeAllPattern = {
    import coop.rchain.models.rholang.{implicits => toPar}
    BindPattern(List(toPar(Expr(EVarBody(EVar(Var(FreeVar(0))))))), freeCount = 1)
  }
}

final class RuntimeOps[F[_]](private val runtime: RhoRuntime[F]) extends AnyVal {

  /**
    * Because of the history legacy, the emptyStateHash does not really represent an empty trie.
    * The `emptyStateHash` is used as genesis block pre state which the state only contains registry
    * fixed channels in the state.
    */
  def emptyStateHash(implicit m: Monad[F]): F[StateHash] =
    for {
      _          <- runtime.reset(emptyRootHash)
      _          <- bootstrapRegistry(runtime)
      checkpoint <- runtime.createCheckpoint
      hash       = ByteString.copyFrom(checkpoint.root.bytes.toArray)
    } yield hash

  /* Compute state with deploys (genesis block) and System deploys (regular block) */

  /**
    * Evaluates deploys and System deploys with checkpoint to get final state hash
    */
  def computeState(
      startHash: StateHash,
      terms: Seq[Signed[DeployData]],
      systemDeploys: Seq[SystemDeploy],
      blockData: BlockData,
      rand: Blake2b512Random
  )(
      implicit s: Sync[F],
      span: Span[F],
      log: Log[F]
  ): F[(StateHash, Seq[UserDeployRuntimeResult], Seq[SystemDeployRuntimeResult])] =
    Span[F].traceI("compute-state") {
      for {
        _ <- runtime.setBlockData(blockData)
        deployProcessResult <- Span[F].withMarks("play-deploys") {
                                playDeploys(
                                  startHash,
                                  terms,
                                  playDeployWithCostAccounting,
                                  rand
                                )
                              }
        (startHash, processedDeploys) = deployProcessResult
        systemDeployProcessResult <- {
          systemDeploys.toList.foldM((startHash, Vector.empty[SystemDeployRuntimeResult])) {
            case ((startHash, processedSystemDeploys), sd) =>
              playSystemDeploy(startHash)(sd) >>= {
                case PlaySucceeded(stateHash, processedSystemDeploy, mergeChs, _) => {
                  val result = SystemDeployRuntimeResult(processedSystemDeploy, mergeChs)
                  (stateHash, processedSystemDeploys :+ result).pure[F]
                }
                case PlayFailed(Failed(_, errorMsg)) => {
                  val errStr = "Unexpected system error during play of system deploy: " + errorMsg
                  new Exception(errStr)
                    .raiseError[F, (StateHash, Vector[SystemDeployRuntimeResult])]
                }
              }
          }
        }
        (postStateHash, processedSystemDeploys) = systemDeployProcessResult
      } yield (postStateHash, processedDeploys, processedSystemDeploys)
    }

  /**
    * Evaluates genesis deploys with checkpoint to get final state hash
    */
  def computeGenesis(
      terms: Seq[Signed[DeployData]],
      blockTime: Long,
      blockNumber: Long,
      shardId: String
  )(
      implicit s: Sync[F],
      span: Span[F],
      log: Log[F]
  ): F[(StateHash, StateHash, Seq[UserDeployRuntimeResult])] =
    Span[F].traceI("compute-genesis") {
      val blockData = BlockData(blockTime, blockNumber, Genesis.genesisPubKey, 0)
      for {
        _                   <- runtime.setBlockData(blockData)
        genesisPreStateHash <- emptyStateHash
        rand = BlockRandomSeed(
          shardId,
          blockNumber,
          Genesis.genesisPubKey,
          Blake2b256Hash.fromByteString(genesisPreStateHash)
        ).generateRandomNumber
        playResult                    <- playDeploys(genesisPreStateHash, terms, processDeployWithMergeableData, rand)
        (stateHash, processedDeploys) = playResult
      } yield (genesisPreStateHash, stateHash, processedDeploys)
    }

  /* Deploy evaluators */

  /**
    * Evaluates deploys on root hash with checkpoint to get final state hash
    */
  def playDeploys(
      startHash: StateHash,
      terms: Seq[Signed[DeployData]],
      processDeploy: (Signed[DeployData], Blake2b512Random) => F[UserDeployRuntimeResult],
      rand: Blake2b512Random
  )(implicit m: Monad[F]): F[(StateHash, Seq[UserDeployRuntimeResult])] =
    for {
      _ <- runtime.reset(startHash.toBlake2b256Hash)
      res <- terms.zipWithIndex.toList.traverse {
              case (d, i) => processDeploy(d, rand.splitByte(i.toByte))
            }
      finalCheckpoint <- runtime.createCheckpoint
      finalStateHash  = finalCheckpoint.root
    } yield (finalStateHash.toByteString, res)

  /**
    * Evaluates deploy with cost accounting (Pos Pre-charge and Refund calls)
    */
  def playDeployWithCostAccounting(
      deploy: Signed[DeployData],
      rand: Blake2b512Random
  )(implicit s: Sync[F], log: Log[F], span: Span[F]): F[UserDeployRuntimeResult] = {
    // Pre-charge system deploy evaluator
    val preChargeF: F[(Vector[Event], Either[SystemDeployUserError, Unit], Set[Par])] =
      playSystemDeployInternal(
        new PreChargeDeploy(
          deploy.data.totalPhloCharge,
          deploy.pk,
          rand.splitByte(BlockRandomSeed.PreChargeSplitIndex)
        )
      )
    // Refund system deploy evaluator
    def refundF(
        amount: Long
    ): F[(Vector[Event], Either[SystemDeployUserError, Unit], Set[Par])] =
      playSystemDeployInternal(
        new RefundDeploy(amount, rand.splitByte(BlockRandomSeed.RefundSplitIndex))
      )

    // Event logs and mergeable channels are accumulated inside local state
    Ref.of(EvalCollector()) flatMap { st =>
      // System deploy result of evaluation
      type R[S <: SystemDeploy] = Either[SystemDeployUserError, S#Result]

      // Combines system deploy evaluation and update of local state with resulting event logs
      def execAndSave[S <: SystemDeploy](
          deployEval: F[(Vector[Event], R[S], Set[Par])]
      ): F[R[S]] =
        for {
          evalResult                   <- deployEval
          (eventLog, result, mergeChs) = evalResult
          // Append event log to local state
          _ <- st.update(_.add(eventLog, mergeChs))
        } yield result

      // Creates Pre-charge with diagnostics
      val preChargeDiag: F[R[PreChargeDeploy]] =
        Log[F].info(
          s"PreCharging ${Base16.encode(deploy.pk.bytes)} for ${deploy.data.totalPhloCharge}"
        ) *> Span[F].traceI("precharge")(execAndSave[PreChargeDeploy](preChargeF))

      // Creates Refund with diagnostics
      def refundDiag(amount: Long): F[R[RefundDeploy]] =
        Log[F].info(s"Refunding ${Base16.encode(deploy.pk.bytes)} with ${amount}") *>
          Span[F].traceI("refund")(execAndSave[RefundDeploy](refundF(amount)))

      // Creates user deploy evaluator with diagnostics
      val userDeployDiag = Span[F].traceI("user-deploy")(
        // Evaluates user deploy and append event log to local state
        processDeploy(deploy, rand.splitByte(BlockRandomSeed.UserDeploySplitIndex)).flatTap {
          case (pd, r) => st.update(_.add(pd.deployLog, r.mergeable))
        }
      )

      // Evaluates Pre-charge system deploy
      EitherT(preChargeDiag)
      // Evaluates user deploy
        .semiflatMap(_ => userDeployDiag)
        .flatTap {
          case (pd, _) =>
            // Evaluates Refund system deploy
            EitherT(refundDiag(pd.refundAmount))
              .leftSemiflatTap { error =>
                // If Pre-charge succeeds and Refund fails, it's a platform error and we should signal it with raiseError
                Log[F].warn(s"Refund failure '${error.errorMessage}'") *>
                  GasRefundFailure(error.errorMessage).raiseError[F, Unit]
              }
        }
        .valueOr {
          // Handle evaluation errors from PreCharge or Refund
          // - assigning 0 cost - replay should reach the same state
          case SystemDeployUserError(errorMsg) =>
            val pd = ProcessedDeploy.empty(deploy).copy(systemDeployError = Some(errorMsg))
            val er = EvaluateResult(cost = Cost(0), errors = Vector(), mergeable = Set())
            (pd, er)
        }
        .flatMap {
          case (pd, evalResult) =>
            // Update result with accumulated event logs (if evaluation failed also)
            for {
              collected             <- st.get
              mergeableChannelsData <- getNumberChannelsData(collected.mergeableChannels)
            } yield UserDeployRuntimeResult(
              pd.copy(deployLog = collected.eventLog.toList),
              mergeableChannelsData,
              evalResult
            )
        }
    }
  }

  /**
    * Evaluates deploy
    */
  def processDeploy(
      deploy: Signed[DeployData],
      rand: Blake2b512Random
  )(implicit s: Sync[F], span: Span[F], log: Log[F]): F[(ProcessedDeploy, EvaluateResult)] =
    Span[F].withMarks("play-deploy") {
      for {
        fallback <- runtime.createSoftCheckpoint

        // Evaluate deploy
        evaluateResult <- evaluate(deploy, rand)

        checkpoint <- runtime.createSoftCheckpoint

        evalSucceeded = evaluateResult.errors.isEmpty
        deployResult = ProcessedDeploy(
          deploy,
          Cost.toProto(evaluateResult.cost),
          checkpoint.log.map(EventConverter.toCasperEvent).toList,
          !evalSucceeded
        )

        _ <- (runtime.revertToSoftCheckpoint(fallback) *>
              printDeployErrors(deploy.sig, evaluateResult.errors)).whenA(!evalSucceeded)

      } yield (deployResult, evaluateResult)
    }

  def processDeployWithMergeableData(
      deploy: Signed[DeployData],
      rand: Blake2b512Random
  )(implicit s: Sync[F], span: Span[F], log: Log[F]): F[UserDeployRuntimeResult] =
    processDeploy(deploy, rand.splitByte(BlockRandomSeed.UserDeploySplitIndex)) flatMap {
      case (pd, result @ EvaluateResult(_, _, mergeChs)) =>
        for {
          mergeableData <- getNumberChannelsData(mergeChs)
        } yield UserDeployRuntimeResult(pd, mergeableData, result)
    }

  def getNumberChannelsData(
      channels: Set[Par]
  )(implicit s: Sync[F]): F[NumberChannelsEndVal] =
    channels.toList.traverse(getNumberChannel).map(_.flatten.toMap)

  def getNumberChannel(chan: Par)(implicit m: Sync[F]): F[Option[(Blake2b256Hash, Long)]] =
    // Read current channel value
    for {
      chValues <- runtime.getData(chan)

      r <- if (chValues.isEmpty) {
            none[(Blake2b256Hash, Long)].pure[F]
          } else {
            for {
              _ <- new Exception(s"NumberChannel must have singleton value.").raiseError
                    .whenA(chValues.size != 1)

              numPar = chValues.head.a

              (num, _) = RholangMergingLogic.getNumberWithRnd(numPar)
              chHash   = StableHashProvider.hash(chan)(storage.serializePar)
            } yield (chHash, num).some
          }
    } yield r

  /* System deploy evaluators */

  /**
    * Evaluates System deploy with checkpoint to get final state hash
    */
  def playSystemDeploy[S <: SystemDeploy](stateHash: StateHash)(
      systemDeploy: S
  )(implicit s: Sync[F], span: Span[F]): F[SystemDeployResult[S#Result]] =
    for {
      _ <- runtime.reset(stateHash.toBlake2b256Hash)

      playResult                       <- playSystemDeployInternal(systemDeploy)
      (eventLog, result, mergeableChs) = playResult

      finalStateHash <- runtime.createCheckpoint.map(_.root.toByteString)

      sysResult <- result match {
                    case Right(result) =>
                      getNumberChannelsData(mergeableChs) map { mcl =>
                        systemDeploy match {
                          case SlashDeploy(slashedValidator, _) =>
                            SystemDeployResult
                              .playSucceeded(
                                finalStateHash,
                                eventLog,
                                SystemDeployData.from(slashedValidator),
                                mcl,
                                result
                              )
                          case CloseBlockDeploy(_) =>
                            SystemDeployResult
                              .playSucceeded(
                                finalStateHash,
                                eventLog,
                                SystemDeployData.from(),
                                mcl,
                                result
                              )
                          // TODO: what is the purpose of empty system deploy?
                          case _ =>
                            SystemDeployResult
                              .playSucceeded(
                                finalStateHash,
                                eventLog,
                                SystemDeployData.empty,
                                mcl,
                                result
                              )
                        }
                      }
                    case Left(userError @ SystemDeployUserError(_)) =>
                      SystemDeployResult.playFailed[S#Result](eventLog, userError).pure
                  }
    } yield sysResult

  def playSystemDeployInternal[S <: SystemDeploy](
      systemDeploy: S
  )(
      implicit s: Sync[F],
      span: Span[F]
  ): F[(Vector[Event], Either[SystemDeployUserError, S#Result], Set[Par])] =
    for {
      // Get System deploy result / throw fatal errors for unexpected results
      result <- evalSystemDeploy(systemDeploy)

      (resultOrSystemDeployError, evalResult) = result
      postDeploySoftCheckpoint                <- runtime.createSoftCheckpoint
      log                                     = postDeploySoftCheckpoint.log
    } yield (
      log.map(EventConverter.toCasperEvent).toVector,
      resultOrSystemDeployError,
      evalResult.mergeable
    )

  /**
    * Evaluates System deploy (applicative errors are fatal)
    */
  def evalSystemDeploy[S <: SystemDeploy](
      systemDeploy: S
  )(implicit m: Sync[F], span: Span[F]): F[SysEvalResult[S]] =
    for {
      // Evaluate Rholang term with trace diagnostics
      evalResult <- Span[F].traceI("evaluate-system-source") {
                     evaluateSystemSource(systemDeploy)
                   }

      // Throw fatal error if Rholang execution failed
      _ <- UnexpectedSystemErrors(evalResult.errors)
            .raiseError[F, SysEvalResult[S]]
            .whenA(evalResult.failed)

      // Consume System deploy result with trace diagnostics
      consumeResultDiag = Span[F].traceI("consume-system-result") {
        consumeSystemResult(systemDeploy)
      }

      // Get Rholang evaluation result
      r <- OptionT(consumeResultDiag).semiflatMap {
            // All other user errors are considered fatal
            case (_, Seq(ListParWithRandom(Seq(par), _))) =>
              // Extract result
              systemDeploy.extractResult[F](par)
            case (_, Seq(ListParWithRandom(pars, _))) =>
              // Fatal error if System deploy returned unexpected results
              UnexpectedResult(pars)
                .raiseError[F, Either[SystemDeployUserError, systemDeploy.Result]]
          } getOrElseF
            // Fatal error is System deploy didn't returned results
            ConsumeFailed.raiseError
    } yield (r, evalResult)

  /**
    * Evaluates exploratory (read-only) deploy
    */
  def playExploratoryDeploy(term: String, hash: StateHash)(implicit s: Sync[F]): F[Seq[Par]] = {
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
    val rand = Blake2b512Random.defaultRandom
    import coop.rchain.models.rholang.implicits._
    val returnName: Par = GPrivate(ByteString.copyFrom(rand.copy().next()))

    // Execute deploy on top of specified block hash
    captureResults(hash, rand, deploy, returnName)
  }

  /* Checkpoints */

  /**
    * Creates soft checkpoint with rollback if result is false.
    */
  def withSoftTransaction[A](fa: F[(A, Boolean)])(implicit m: Monad[F]): F[A] =
    for {
      fallback <- runtime.createSoftCheckpoint
      // Execute action
      result       <- fa
      (a, success) = result
      // Revert the state if failed
      _ <- runtime.revertToSoftCheckpoint(fallback).whenA(!success)
    } yield a

  /* Evaluates and captures results */

  // Return channel on which result is captured is the first name
  // in the deploy term `new return in { return!(42) }`
  def captureResults(
      start: StateHash,
      deploy: Signed[DeployData]
  )(implicit s: Sync[F]): F[Seq[Par]] = {
    // Create return channel as first unforgeable name created in deploy term
    val rand = Blake2b512Random.defaultRandom
    import coop.rchain.models.rholang.implicits._
    val returnName: Par = GPrivate(ByteString.copyFrom(rand.copy().next()))
    captureResults(start, rand, deploy, returnName)
  }

  def captureResults(
      start: StateHash,
      rand: Blake2b512Random,
      deploy: Signed[DeployData],
      name: Par
  )(
      implicit s: Sync[F]
  ): F[Seq[Par]] =
    captureResultsWithErrors(start, rand, deploy, name)
      .handleErrorWith(
        ex =>
          BugFoundError(s"Unexpected error while capturing results from Rholang: $ex")
            .raiseError[F, Seq[Par]]
      )

  def captureResultsWithErrors(
      start: StateHash,
      rand: Blake2b512Random,
      deploy: Signed[DeployData],
      name: Par
  )(implicit s: Sync[F]): F[Seq[Par]] =
    runtime.reset(start.toBlake2b256Hash) >>
      evaluate(deploy, rand)
        .flatMap({ res =>
          if (res.errors.nonEmpty) Sync[F].raiseError[EvaluateResult](res.errors.head)
          else res.pure[F]
        }) >> getDataPar(name)

  def evaluate(deploy: Signed[DeployData], rand: Blake2b512Random): F[EvaluateResult] = {
    import coop.rchain.models.rholang.implicits._
    runtime.evaluate(
      deploy.data.term,
      Cost(deploy.data.phloLimit),
      NormalizerEnv(deploy).toEnv,
      rand
    )
  }

  def evaluateSystemSource[S <: SystemDeploy](systemDeploy: S): F[EvaluateResult] =
    runtime.evaluate(systemDeploy.source, Cost.UNSAFE_MAX, systemDeploy.env, systemDeploy.rand)

  def getDataPar(channel: Par)(implicit f: Functor[F]): F[Seq[Par]] =
    runtime.getData(channel).map(_.flatMap(_.a.pars))

  def getContinuationPar(
      channels: Seq[Par]
  )(implicit f: Functor[F]): F[Seq[(Seq[BindPattern], Par)]] =
    runtime
      .getContinuation(channels)
      .map(
        _.filter(_.continuation.taggedCont.isParBody)
          .map(result => (result.patterns, result.continuation.taggedCont.parBody.get.body))
      )

  def consumeResult(
      channel: Par,
      pattern: BindPattern
  ): F[Option[(TaggedContinuation, Seq[ListParWithRandom])]] =
    runtime.consumeResult(Seq(channel), Seq(pattern))

  def consumeSystemResult[S <: SystemDeploy](
      systemDeploy: SystemDeploy
  ): F[Option[(TaggedContinuation, Seq[ListParWithRandom])]] =
    consumeResult(systemDeploy.returnChannel, systemDeployConsumeAllPattern)

  /* Read only Rholang evaluator helpers */

  def getActiveValidators(startHash: StateHash)(implicit s: Sync[F]): F[Seq[Validator]] =
    playExploratoryDeploy(activateValidatorQuerySource, startHash)
      .ensureOr(
        validatorsPar =>
          new IllegalArgumentException(
            s"Incorrect number of results from query of current active validator in state ${PrettyPrinter
              .buildString(startHash)}: ${validatorsPar.size}"
          )
      )(validatorsPar => validatorsPar.size == 1)
      .map(validatorsPar => toValidatorSeq(validatorsPar.head))

  def computeBonds(hash: StateHash)(implicit s: Sync[F]): F[Seq[Bond]] =
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

  private def activateValidatorQuerySource: String =
    s"""
       # new return, rl(`rho:registry:lookup`), poSCh in {
       #   rl!(`rho:rchain:pos`, *poSCh) |
       #   for(@(_, Pos) <- poSCh) {
       #     @Pos!("getActiveValidators", *return)
       #   }
       # }
       """.stripMargin('#')

  private def bondsQuerySource: String =
    s"""
       # new return, rl(`rho:registry:lookup`), poSCh in {
       #   rl!(`rho:rchain:pos`, *poSCh) |
       #   for(@(_, Pos) <- poSCh) {
       #     @Pos!("getBonds", *return)
       #   }
       # }
       """.stripMargin('#')

  private def toValidatorSeq(validatorsPar: Par): Seq[Validator] =
    validatorsPar.exprs.head.getESetBody.ps.map { validator =>
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
}
