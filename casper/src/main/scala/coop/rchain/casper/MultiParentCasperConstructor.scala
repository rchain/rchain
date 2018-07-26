package coop.rchain.casper

import cats.implicits._
import cats.{Applicative, Monad}
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.BlockStore
import coop.rchain.casper.Estimator.BlockHash
import coop.rchain.catscontrib._
import coop.rchain.casper.genesis.Genesis
import coop.rchain.casper.protocol.ApprovedBlock
import coop.rchain.casper.util.rholang.RuntimeManager
import coop.rchain.casper.protocol.{ApprovedBlock, ApprovedBlockCandidate, BlockMessage}
import coop.rchain.comm.CommError.ErrorHandler
import coop.rchain.comm.discovery._
import coop.rchain.comm.transport._
import coop.rchain.shared.{Log, LogSource, Time}
import monix.execution.Scheduler

import scala.concurrent.{Future, Promise}

trait MultiParentCasperConstructor[F[_]] {
  def casperInstance: Either[Throwable, MultiParentCasper[F]]
  def receive(message: ApprovedBlock): F[Boolean]
  def lastApprovedBlock: F[Option[ApprovedBlock]]
}

object MultiParentCasperConstructor extends MultiParentCasperConstructorInstances {
  def apply[F[_]](
      implicit instance: MultiParentCasperConstructor[F]): MultiParentCasperConstructor[F] =
    instance
}

sealed abstract class MultiParentCasperConstructorInstances {
  private implicit val logSource: LogSource = LogSource(this.getClass)

  def failureCasperConstructor[F[_]: Applicative]: MultiParentCasperConstructor[F] =
    new MultiParentCasperConstructor[F] {
      override def casperInstance: Either[Throwable, MultiParentCasper[F]] =
        Left(new Exception(
          "Failure instance of Casper Constructor can never create a MultiParentCasper instance."))

      override def receive(message: ApprovedBlock): F[Boolean] = false.pure[F]
      override def lastApprovedBlock: F[Option[ApprovedBlock]] = none.pure[F]
    }

  def successCasperConstructor[F[_]: Applicative](
      genesis: ApprovedBlock,
      casper: MultiParentCasper[F]): MultiParentCasperConstructor[F] =
    new MultiParentCasperConstructor[F] {
      override def receive(message: ApprovedBlock): F[Boolean] = false.pure[F]

      override def casperInstance: Either[Throwable, MultiParentCasper[F]] = Right(casper)

      override def lastApprovedBlock: F[Option[ApprovedBlock]] = genesis.some.pure[F]
    }

  def awaitApprovedBlock[
      F[_]: Monad: Capture: NodeDiscovery: TransportLayer: Log: Time: ErrorHandler: SafetyOracle: BlockStore](
      runtimeManager: RuntimeManager,
      validatorId: Option[ValidatorIdentity],
      validators: Set[ByteString])(implicit scheduler: Scheduler): MultiParentCasperConstructor[F] =
    new MultiParentCasperConstructor[F] {
      private val genesisPromise = Promise[(ApprovedBlock, Map[BlockHash, BlockMessage])]()
      private val casper: Future[MultiParentCasper[F]] =
        genesisPromise.future.map(
          g =>
            MultiParentCasper.hashSetCasper[F](
              runtimeManager,
              validatorId,
              g._1.candidate.get.block.get,
              g._2
          ))

      override def receive(a: ApprovedBlock): F[Boolean] =
        //TODO: Allow update to more recent ApprovedBlock
        if (genesisPromise.isCompleted) false.pure[F]
        else
          for {
            isValid <- Validate.approvedBlock[F](a, validators)
            _ <- if (isValid) for {
                  _           <- Log[F].info("CASPER: Valid ApprovedBlock received!")
                  genesis     = a.candidate.get.block.get
                  _           <- BlockStore[F].put(genesis.blockHash, genesis)
                  internalMap <- BlockStore[F].asMap()
                  _           = genesisPromise.success((a, internalMap))
                } yield ()
                else Log[F].info("CASPER: Invalid ApprovedBlock received; refusing to add.")
          } yield isValid

      override def casperInstance: Either[Throwable, MultiParentCasper[F]] =
        casper.value.fold[Either[Throwable, MultiParentCasper[F]]](
          Left(new Exception(
            "No valid ApprovedBlock has been received to instantiate a Casper instance!")))(
          _.toEither)

      override def lastApprovedBlock: F[Option[ApprovedBlock]] =
        genesisPromise.future.value.flatMap(_.toOption.map(_._1)).pure[F]
    }

  def fromConfig[
      F[_]: Monad: Capture: NodeDiscovery: TransportLayer: Log: Time: ErrorHandler: SafetyOracle: BlockStore,
      G[_]: Monad: Capture: Log: Time: BlockStore](conf: CasperConf,
                                                   runtimeManager: RuntimeManager)(
      implicit scheduler: Scheduler): G[MultiParentCasperConstructor[F]] =
    if (conf.createGenesis) {
      for {
        genesis <- Genesis.fromInputFiles[G](conf.bondsFile,
                                             conf.numValidators,
                                             conf.genesisPath,
                                             conf.walletsFile,
                                             runtimeManager)
        candidate   = ApprovedBlockCandidate(block = Some(genesis), requiredSigs = 0)
        approved    = ApprovedBlock(candidate = Some(candidate)) //TODO: do actual approval protocol
        validatorId <- ValidatorIdentity.fromConfig[G](conf)
        _           <- BlockStore[G].put(genesis.blockHash, genesis)
        internalMap <- BlockStore[G].asMap()
        casper = MultiParentCasper
          .hashSetCasper[F](runtimeManager, validatorId, genesis, internalMap)
      } yield {
        successCasperConstructor[F](approved, casper)
      }
    } else {
      for {
        validators  <- CasperConf.parseValidatorsFile[G](conf.knownValidatorsFile)
        validatorId <- ValidatorIdentity.fromConfig[G](conf)
      } yield awaitApprovedBlock[F](runtimeManager, validatorId, validators)
    }

  def withCasper[F[_]: Monad: Log: MultiParentCasperConstructor, A](f: MultiParentCasper[F] => F[A],
                                                                    default: A): F[A] =
    MultiParentCasperConstructor[F].casperInstance match {
      case Right(casper) => f(casper)
      case Left(ex) =>
        Log[F].warn(s"Casper instance was not available: ${ex.getMessage}").map(_ => default)
    }

}
