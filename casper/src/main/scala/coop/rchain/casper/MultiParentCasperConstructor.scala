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
import coop.rchain.shared.{Log, LogSource, Time}

import monix.execution.Scheduler

import scala.concurrent.{Future, Promise}

trait MultiParentCasperConstructor[F[_]] {
  def casperInstance: Either[Throwable, MultiParentCasper[F]]
  def receive(message: ApprovedBlock): F[Boolean]
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
    }

  def successCasperConstructor[F[_]: Applicative](
      casper: MultiParentCasper[F]): MultiParentCasperConstructor[F] =
    new MultiParentCasperConstructor[F] {
      override def receive(message: ApprovedBlock): F[Boolean] = false.pure[F]

      override def casperInstance: Either[Throwable, MultiParentCasper[F]] = Right(casper)
    }

  def awaitApprovedBlock[
      F[_]: Monad: Capture: NodeDiscovery: TransportLayer: Log: Time: ErrorHandler: SafetyOracle](
      activeRuntime: Runtime,
      validatorId: Option[ValidatorIdentity],
      validators: Set[ByteString])(implicit scheduler: Scheduler): MultiParentCasperConstructor[F] =
    new MultiParentCasperConstructor[F] {
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
      implicit scheduler: Scheduler): G[MultiParentCasperConstructor[F]] =
    for {
      validators  <- CasperConf.parseValidatorsFile[G](conf.knownValidatorsFile)
      validatorId <- ValidatorIdentity.fromConfig[G](conf)
    } yield awaitApprovedBlock[F](activeRuntime, validatorId, validators)

  def withCasper[F[_]: Applicative: Log: MultiParentCasperConstructor, A](
      f: MultiParentCasper[F] => F[A],
      default: A): F[A] = MultiParentCasperConstructor[F].casperInstance match {
    case Right(casper) => f(casper)
    case Left(ex) =>
      Log[F].warn(s"Casper instance was not available: ${ex.getMessage}").map(_ => default)
  }

}
