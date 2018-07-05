package coop.rchain.casper

import cats.{Applicative, Monad}
import cats.implicits._

import com.google.protobuf.ByteString

import coop.rchain.catscontrib._
import coop.rchain.casper.genesis.Genesis
import coop.rchain.casper.protocol.{ApprovedBlock, BlockMessage}
import coop.rchain.comm.CommError.ErrorHandler
import coop.rchain.comm.transport._
import coop.rchain.comm.discovery._
import coop.rchain.rholang.interpreter.Runtime
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
      override def lastApprovedBlock: F[Option[ApprovedBlock]]             = genesis.some.pure[F]
    }

  def awaitApprovedBlock[
      F[_]: Monad: Capture: NodeDiscovery: TransportLayer: Log: Time: ErrorHandler: SafetyOracle](
      activeRuntime: Runtime,
      validatorId: Option[ValidatorIdentity],
      validators: Set[ByteString])(implicit scheduler: Scheduler): MultiParentCasperConstructor[F] =
    new MultiParentCasperConstructor[F] {
      private val genesis = Promise[ApprovedBlock]()
      private val casper: Future[MultiParentCasper[F]] =
        genesis.future.map(
          g =>
            MultiParentCasper.hashSetCasper[F](
              activeRuntime: Runtime,
              validatorId,
              g.block.get
          ))

      override def receive(a: ApprovedBlock): F[Boolean] =
        //TODO: Allow update to more recent ApprovedBlock
        if (genesis.isCompleted) false.pure[F]
        else
          for {
            isVaid <- Validate.approvedBlock[F](a, validators)
            _ <- if (isVaid)
                  Log[F].info("CASPER: Valid ApprovedBlock received!") *> Capture[F].capture {
                    genesis.success(a)
                  }.void
                else Log[F].info("CASPER: Invalid ApprovedBlock received; refusing to add.")
          } yield isVaid

      override def casperInstance: Either[Throwable, MultiParentCasper[F]] =
        casper.value.fold[Either[Throwable, MultiParentCasper[F]]](
          Left(new Exception(
            "No valid ApprovedBlock has been received to instantiate a Casper instance!")))(
          _.toEither)

      override def lastApprovedBlock: F[Option[ApprovedBlock]] =
        genesis.future.value.flatMap(_.toOption).pure[F]
    }

  def fromConfig[
      F[_]: Monad: Capture: NodeDiscovery: TransportLayer: Log: Time: ErrorHandler: SafetyOracle,
      G[_]: Monad: Capture: Log: Time](conf: CasperConf, activeRuntime: Runtime)(
      implicit scheduler: Scheduler): G[MultiParentCasperConstructor[F]] =
    if (conf.createGenesis) {
      for {
        genesis <- Genesis.fromInputFiles[G](conf.bondsFile,
                                             conf.numValidators,
                                             conf.genesisPath,
                                             conf.walletsFile,
                                             activeRuntime)
        approved    = ApprovedBlock(block = Some(genesis)) //TODO: do actual approval protocol
        validatorId <- ValidatorIdentity.fromConfig[G](conf)
        casper      = MultiParentCasper.hashSetCasper[F](activeRuntime, validatorId, genesis)
      } yield successCasperConstructor[F](approved, casper)
    } else {
      for {
        validators  <- CasperConf.parseValidatorsFile[G](conf.knownValidatorsFile)
        validatorId <- ValidatorIdentity.fromConfig[G](conf)
      } yield awaitApprovedBlock[F](activeRuntime, validatorId, validators)
    }

  def withCasper[F[_]: Applicative: Log: MultiParentCasperConstructor, A](
      f: MultiParentCasper[F] => F[A],
      default: A): F[A] = MultiParentCasperConstructor[F].casperInstance match {
    case Right(casper) => f(casper)
    case Left(ex) =>
      Log[F].warn(s"Casper instance was not available: ${ex.getMessage}").map(_ => default)
  }

}
