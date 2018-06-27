package coop.rchain.casper

import cats.{Applicative, Monad}
import cats.implicits._

import com.google.protobuf.ByteString

import coop.rchain.catscontrib._
import coop.rchain.casper.protocol.{ApprovedBlock, BlockMessage}
import coop.rchain.comm.CommError.ErrorHandler
import coop.rchain.comm.transport._
import coop.rchain.comm.discovery._
import coop.rchain.rholang.interpreter.Runtime
import coop.rchain.shared.{Log, Time}

import monix.execution.Scheduler

import scala.concurrent.{Future, Promise}

trait MultiParentCasperConstructor[F[_], A] {
  def casperInstance: Either[Throwable, MultiParentCasper[F]]
  def receive(message: A): F[Boolean]
}

object MultiParentCasperConstructor extends MultiParentCasperConstructorInstances {
  def apply[F[_], A](
      implicit instance: MultiParentCasperConstructor[F, A]): MultiParentCasperConstructor[F, A] =
    instance
}

sealed abstract class MultiParentCasperConstructorInstances {

  def failureCasperConstructor[F[_]: Applicative]: MultiParentCasperConstructor[F, Unit] =
    new MultiParentCasperConstructor[F, Unit] {
      override def casperInstance: Either[Throwable, MultiParentCasper[F]] =
        Left(new Exception(
          "Failure instance of Casper Constructor can never create a MultiParentCasper instance."))

      override def receive(message: Unit): F[Boolean] = false.pure[F]
    }

  def successCasperConstructor[F[_]: Applicative](
      casper: MultiParentCasper[F]): MultiParentCasperConstructor[F, ApprovedBlock] =
    new MultiParentCasperConstructor[F, ApprovedBlock] {
      override def receive(message: ApprovedBlock): F[Boolean] = false.pure[F]

      override def casperInstance: Either[Throwable, MultiParentCasper[F]] = Right(casper)
    }

  def awaitApprovedBlock[
      F[_]: Monad: Capture: NodeDiscovery: TransportLayer: Log: Time: ErrorHandler: SafetyOracle](
      activeRuntime: Runtime,
      validatorId: Option[ValidatorIdentity],
      validators: Set[ByteString])(
      implicit scheduler: Scheduler): MultiParentCasperConstructor[F, ApprovedBlock] =
    new MultiParentCasperConstructor[F, ApprovedBlock] {
      private val genesis = Promise[BlockMessage]()
      private val casper: Future[MultiParentCasper[F]] =
        genesis.future.map(
          g =>
            MultiParentCasper.hashSetCasper[F](
              activeRuntime: Runtime,
              validatorId,
              g
          ))

      override def receive(a: ApprovedBlock): F[Boolean] =
        for {
          isVaid <- Validate.approvedBlock[F](a, validators)
          _ <- if (isVaid) Capture[F].capture { genesis.success(a.block.get) }.void
              else ().pure[F]
        } yield isVaid

      override def casperInstance: Either[Throwable, MultiParentCasper[F]] =
        casper.value.fold[Either[Throwable, MultiParentCasper[F]]](
          Left(new Exception(
            "No valid ApprovedBlock has been received to instantiate a Casper instance!")))(
          _.toEither)
    }

  def fromConfig[
      F[_]: Monad: Capture: NodeDiscovery: TransportLayer: Log: Time: ErrorHandler: SafetyOracle,
      G[_]: Monad: Capture: Log](conf: CasperConf, activeRuntime: Runtime)(
      implicit scheduler: Scheduler): G[MultiParentCasperConstructor[F, ApprovedBlock]] =
    for {
      validators  <- CasperConf.parseValidatorsFile[G](conf.knownValidatorsFile)
      validatorId <- ValidatorIdentity.fromConfig[G](conf)
    } yield awaitApprovedBlock[F](activeRuntime, validatorId, validators)
}
