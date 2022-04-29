package coop.rchain.casper.rholang.syntax

import cats.data.EitherT
import cats.effect.Sync
import cats.effect.concurrent.Ref
import cats.syntax.all._
import com.google.protobuf.ByteString
import coop.rchain.casper.CasperMetricsSource
import coop.rchain.casper.protocol.{
  CloseBlockSystemDeployData,
  Empty,
  ProcessedDeploy,
  ProcessedSystemDeploy,
  SlashSystemDeployData
}
import coop.rchain.casper.rholang.InterpreterUtil.printDeployErrors
import coop.rchain.casper.rholang._
import coop.rchain.casper.rholang.syntax.RuntimeSyntax.SysEvalResult
import coop.rchain.casper.rholang.sysdeploys.{
  CloseBlockDeploy,
  PreChargeDeploy,
  RefundDeploy,
  SlashDeploy
}
import coop.rchain.casper.rholang.types.{
  ReplayFailure,
  SystemDeploy,
  SystemDeployUserError,
  UnusedCOMMEvent
}
import coop.rchain.casper.util.EventConverter
import coop.rchain.catscontrib.Catscontrib._
import coop.rchain.metrics.implicits._
import coop.rchain.metrics.{Metrics, Span}
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.models.Par
import coop.rchain.models.Validator.Validator
import coop.rchain.models.block.StateHash.StateHash
import coop.rchain.models.syntax._
import coop.rchain.rholang.interpreter.SystemProcesses.BlockData
import coop.rchain.rholang.interpreter.{EvaluateResult, ReplayRhoRuntime}
import coop.rchain.rspace.hashing.Blake2b256Hash
import coop.rchain.rspace.merger.MergingLogic.NumberChannelsEndVal
import coop.rchain.rspace.util.ReplayException
import coop.rchain.shared.Log

trait RuntimeReplaySyntax {
  implicit final def casperSyntaxRholangRuntimeReplay[F[_]: Sync: Span: Log](
      runtime: ReplayRhoRuntime[F]
  ): RuntimeReplayOps[F] =
    new RuntimeReplayOps[F](runtime)
}

final class RuntimeReplayOps[F[_]: Sync: Span: Log](
    private val runtime: ReplayRhoRuntime[F]
) extends RuntimeSyntax {

  implicit val RuntimeMetricsSource = Metrics.Source(CasperMetricsSource, "replay-rho-runtime")

  /* REPLAY Compute state with deploys (genesis block) and System deploys (regular block) */

  /**
    * Evaluates (and validates) deploys and System deploys with checkpoint to valiate final state hash
    */
  def replayComputeState(startHash: StateHash)(
      terms: Seq[ProcessedDeploy],
      systemDeploys: Seq[ProcessedSystemDeploy],
      blockData: BlockData,
      invalidBlocks: Map[BlockHash, Validator] = Map.empty[BlockHash, Validator],
      isGenesis: Boolean //FIXME have a better way of knowing this. Pass the replayDeploy function maybe?
  ): F[Either[ReplayFailure, (Blake2b256Hash, Seq[NumberChannelsEndVal])]] =
    Span[F].traceI("replay-compute-state") {
      for {
        _ <- runtime.setBlockData(blockData)
        _ <- runtime.setInvalidBlocks(invalidBlocks)
        result <- replayDeploys(
                   startHash,
                   terms,
                   systemDeploys,
                   replayDeployE(withCostAccounting = !isGenesis)(_).value,
                   replayBlockSystemDeployDiag(blockData)
                 )
      } yield result
    }

  /* REPLAY Deploy evaluators */

  /**
    * Evaluates (and validates) deploys on root hash with checkpoint to validate final state hash
    */
  def replayDeploys(
      startHash: StateHash,
      terms: Seq[ProcessedDeploy],
      systemDeploys: Seq[ProcessedSystemDeploy],
      replayDeploy: ProcessedDeploy => F[Either[ReplayFailure, NumberChannelsEndVal]],
      replaySystemDeploy: ProcessedSystemDeploy => F[Either[ReplayFailure, NumberChannelsEndVal]]
  ): F[Either[ReplayFailure, (Blake2b256Hash, Vector[NumberChannelsEndVal])]] = {
    type Params[D] = (Seq[D], Vector[NumberChannelsEndVal])

    val deploys = (terms, Vector[NumberChannelsEndVal]()).tailRecM {
      case (Seq(), mergeable) =>
        mergeable.asRight[ReplayFailure].asRight[Params[ProcessedDeploy]].pure[F]
      case (ts, mergeable) =>
        Span[F].traceI("replay-deploy") {
          replayDeploy(ts.head).map { a =>
            a.map(x => (ts.tail, mergeable :+ x)).swap.map(_.asLeft[Vector[NumberChannelsEndVal]])
          }
        }
    }
    val sysDeploys = (systemDeploys, Vector[NumberChannelsEndVal]()).tailRecM {
      case (Seq(), mergeable) =>
        mergeable.asRight[ReplayFailure].asRight[Params[ProcessedSystemDeploy]].pure[F]
      case (ts, mergeable) =>
        Span[F].traceI("replay-sys-deploy") {
          replaySystemDeploy(ts.head).map { a =>
            a.map(x => (ts.tail, mergeable :+ x)).swap.map(_.asLeft[Vector[NumberChannelsEndVal]])
          }
        }
    }
    val refT = Ref.of(Vector[NumberChannelsEndVal]()).liftEitherT[ReplayFailure]

    refT.flatMap { mergeable =>
      EitherT
        .liftF(runtime.reset(startHash.toBlake2b256Hash))
        .flatMap(_ => EitherT(deploys).semiflatTap(chs => mergeable.update(_ ++ chs)))
        .flatMap(_ => EitherT(sysDeploys).semiflatTap(chs => mergeable.update(_ ++ chs)))
        .semiflatMap(_ => mergeable.get)
        .semiflatMap { allMergeable =>
          Span[F].traceI("create-checkpoint") {
            runtime.createCheckpoint.map(c => (c.root, allMergeable))
          }
        }
    }.value
  }

  /**
    * REPLAY Evaluates deploy
    */
  def replayDeploy(withCostAccounting: Boolean)(
      processedDeploy: ProcessedDeploy
  ): F[Option[ReplayFailure]] =
    replayDeployE(withCostAccounting)(processedDeploy).swap.toOption.value

  def replayDeployE(withCostAccounting: Boolean)(
      processedDeploy: ProcessedDeploy
  ): EitherT[F, ReplayFailure, NumberChannelsEndVal] = {
    val refT = Ref.of(Set[Par]()).liftEitherT[ReplayFailure]
    refT flatMap { mergeable =>
      val expectedFailure = processedDeploy.systemDeployError
      val preChargeF =
        Span[F].mark("precharge-started").liftEitherT[ReplayFailure] *>
          replaySystemDeployInternal(
            new PreChargeDeploy(
              processedDeploy.deploy.data.totalPhloCharge,
              processedDeploy.deploy.pk,
              SystemDeployUtil.generatePreChargeDeployRandomSeed(processedDeploy.deploy)
            ),
            expectedFailure
          ).semiflatTap {
            case (_, evalResult) =>
              for {
                _ <- Span[F].mark("precharge-done")
                _ <- runtime.createSoftCheckpoint

                // Collect Pre-charge mergeable channels
                _ <- mergeable.update(_ ++ evalResult.mergeable).whenA(evalResult.succeeded)
              } yield ()
          }

      val refundF =
        Span[F].mark("refund-started").liftEitherT[ReplayFailure] *>
          replaySystemDeployInternal(
            new RefundDeploy(
              processedDeploy.refundAmount,
              SystemDeployUtil.generateRefundDeployRandomSeed(processedDeploy.deploy)
            ),
            None
          ).semiflatTap {
            case (_, evalResult) =>
              for {
                _ <- Span[F].mark("refund-done")
                _ <- runtime.createSoftCheckpoint

                // Collect Refund mergeable channels
                _ <- mergeable.update(_ ++ evalResult.mergeable).whenA(evalResult.succeeded)
              } yield ()

          }

      val deployEvaluator = EitherT
        .liftF {
          runtime.withSoftTransaction {
            for {
              result <- runtime.evaluate(processedDeploy.deploy)

              logErrors = printDeployErrors(processedDeploy.deploy.sig, result.errors)
              /* Since the state of `replaySpace` is reset on each invocation of `replayComputeState`,
              and `ReplayFailure`s mean that block processing is cancelled upstream, we only need to
              reset state if the replay effects of valid deploys need to be discarded. */
              _ <- logErrors.whenA(result.failed)

              // Collect user deploy mergeable channels
              _ <- mergeable.update(_ ++ result.mergeable).whenA(result.succeeded)
            } yield (result, result.succeeded)
          }
        }
        .ensureOr { result =>
          // Regardless of success or failure, verify that deploy status' match.
          ReplayFailure.replayStatusMismatch(processedDeploy.isFailed, result.failed)
        }(result => processedDeploy.isFailed == result.failed)
        .ensureOr { result =>
          // Verify evaluation costs match.
          ReplayFailure.replayCostMismatch(processedDeploy.cost.cost, result.cost.value)
        }(result => processedDeploy.cost.cost == result.cost.value)

      def evaluatorT: EitherT[F, ReplayFailure, Boolean] =
        if (withCostAccounting) {
          preChargeF
            .flatMap { _ =>
              if (expectedFailure.isEmpty)
                deployEvaluator
                  .semiflatMap { evalResult =>
                    for {
                      _ <- Span[F].mark("deploy-eval-done")
                      _ <- runtime.createSoftCheckpoint.whenA(evalResult.succeeded)
                      _ <- Span[F].mark("deploy-done")
                    } yield evalResult.succeeded
                  }
                  .flatTap(_ => refundF)
              else EitherT.rightT(true)
            }
        } else deployEvaluator.map(_.succeeded)

      rigWithCheck(processedDeploy, evaluatorT.map(((), _)).value).semiflatMap { _ =>
        for {
          collected             <- mergeable.get
          mergeableChannelsData <- runtime.getNumberChannelsData(collected)
        } yield mergeableChannelsData
      }
    }
  }

  /* REPLAY System deploy evaluators */

  /**
    * Evaluates System deploy with checkpoint to get final state hash
    */
  def replayBlockSystemDeployDiag(blockData: BlockData)(
      processedSystemDeploy: ProcessedSystemDeploy
  ): F[Either[ReplayFailure, NumberChannelsEndVal]] =
    Span[F].withMarks("replay-system-deploy")(
      replayBlockSystemDeploy(blockData)(processedSystemDeploy).value
    )

  def replayBlockSystemDeploy(blockData: BlockData)(
      processedSysDeploy: ProcessedSystemDeploy
  ): EitherT[F, ReplayFailure, NumberChannelsEndVal] = {
    import processedSysDeploy._
    val sender = ByteString.copyFrom(blockData.sender.bytes)
    systemDeploy match {
      case SlashSystemDeployData(invalidBlockHash, issuerPublicKey) =>
        val slashDeploy = {
          SlashDeploy(
            invalidBlockHash,
            issuerPublicKey,
            SystemDeployUtil.generateSlashDeployRandomSeed(sender, blockData.seqNum)
          )
        }
        rigWithCheck(
          processedSysDeploy,
          replaySystemDeployInternal(slashDeploy, none).semiflatMap {
            case (_, er) =>
              runtime.createSoftCheckpoint.whenA(er.succeeded) *>
                runtime.getNumberChannelsData(er.mergeable).map((_, er))
          }
        ).map(_._1)
      case CloseBlockSystemDeployData =>
        val closeBlockDeploy = CloseBlockDeploy(
          SystemDeployUtil.generateCloseDeployRandomSeed(sender, blockData.seqNum)
        )
        rigWithCheck(
          processedSysDeploy,
          replaySystemDeployInternal(closeBlockDeploy, none).semiflatMap {
            case (_, er) =>
              runtime.createSoftCheckpoint.whenA(er.succeeded) *>
                runtime.getNumberChannelsData(er.mergeable).map((_, er))
          }
        ).map(_._1)
      case Empty =>
        EitherT.leftT(ReplayFailure.internalError(new Exception("Expected system deploy")))
    }
  }

  def replaySystemDeployInternal[S <: SystemDeploy](
      systemDeploy: S,
      expectedFailureMsg: Option[String]
  ): EitherT[F, ReplayFailure, SysEvalResult[S]] = {
    // Evaluate system deploy
    val fe = runtime
      .evalSystemDeploy(systemDeploy)
      .map {
        // Compare evaluation from play and replay, successful or failed
        case (result, evalRes) =>
          (expectedFailureMsg, result) match {
            // Valid replay
            case (None, r @ Right(_)) =>
              // Replayed successful execution
              (r, evalRes).asRight
            case (Some(expectedError), r @ Left(SystemDeployUserError(actualError)))
                if expectedError == actualError =>
              // Replayed failed execution
              (r, evalRes).asRight

            // Invalid replay
            case (Some(expectedError), Left(SystemDeployUserError(actualError))) =>
              // Error messages different
              ReplayFailure.systemDeployErrorMismatch(expectedError, actualError).asLeft
            case (Some(expectedError @ _), Right(result @ _)) =>
              // Error expected, replay successful
              ReplayFailure.replayStatusMismatch(initialFailed = true, replayFailed = false).asLeft
            case (None, Left(actualError @ _)) =>
              // No error expected, replay failed
              ReplayFailure.replayStatusMismatch(initialFailed = false, replayFailed = true).asLeft
          }
      }
    EitherT(fe)
  }

  /* Helper functions */

  def rigWithCheck[A](
      processedDeploy: ProcessedDeploy,
      action: F[Either[ReplayFailure, (A, Boolean)]]
  ): EitherT[F, ReplayFailure, (A, Boolean)] = EitherT(rig(processedDeploy) *> action) flatMap {
    case r @ (_, evalRes) => checkReplayDataWithFix(evalRes).as(r)
  }

  def rigWithCheck[A](
      processedSystemDeploy: ProcessedSystemDeploy,
      action: EitherT[F, ReplayFailure, (A, EvaluateResult)]
  ): EitherT[F, ReplayFailure, (A, EvaluateResult)] =
    rig(processedSystemDeploy).liftEitherT[ReplayFailure] *> action flatMap {
      case r @ (_, evalRes) => checkReplayDataWithFix(evalRes.succeeded).as(r)
    }

  def rig(processedDeploy: ProcessedDeploy): F[Unit] =
    runtime.rig(processedDeploy.deployLog.map(EventConverter.toRspaceEvent))

  def rig(processedSystemDeploy: ProcessedSystemDeploy): F[Unit] =
    runtime.rig(processedSystemDeploy.eventList.map(EventConverter.toRspaceEvent))

  def checkReplayDataWithFix(evalSuccessful: Boolean): EitherT[F, ReplayFailure, Unit] =
    runtime.checkReplayData.attemptT
      .leftMap {
        case replayException: ReplayException =>
          ReplayFailure.unusedCOMMEvent(replayException)
        case throwable => ReplayFailure.internalError(throwable)
      }
      .leftFlatMap {
        case UnusedCOMMEvent(_) if !evalSuccessful =>
          // TODO: temp fix for replay error mismatch
          // https://rchain.atlassian.net/browse/RCHAIN-3505
          EitherT.rightT[F, ReplayFailure](())
        case ex: ReplayFailure => EitherT.leftT[F, Unit](ex)
      }

}
