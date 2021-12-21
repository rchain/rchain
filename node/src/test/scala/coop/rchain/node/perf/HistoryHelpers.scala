package coop.rchain.node.perf

import cats.Parallel
import cats.effect.{Concurrent, ContextShift, Sync}
import coop.rchain.metrics.{Metrics, Span}
import coop.rchain.models.{BindPattern, ListParWithRandom}
import coop.rchain.rholang.interpreter.storage
import coop.rchain.rholang.interpreter.storage.matchListPar
import coop.rchain.rspace
import coop.rchain.rspace.hashing.Blake2b256Hash
import coop.rchain.shared.{Log, Stopwatch}

import scala.concurrent.ExecutionContext

trait HistoryHelpers[F[_]] {

  type Deps =
    (ExecutionContext, Concurrent[F], ContextShift[F], Parallel[F], Log[F], Metrics[F], Span[F])

  def init: Deps

  implicit val (ec, cc, cs, par, lg, mt, sp) = init

  val legacySource: Boolean = false

  def log(msg: String)      = Sync[F].delay(println(msg))
  def shortLog(msg: String) = Sync[F].delay(print(msg + ", "))

  val concurrency = Runtime.getRuntime.availableProcessors()

  // For state items validation
  implicit val codecPar                                           = storage.serializePar
  implicit val codecBind                                          = storage.serializeBindPattern
  implicit val codecPars                                          = storage.serializePars
  implicit val codecCont                                          = storage.serializeTaggedContinuation
  implicit val m: rspace.Match[F, BindPattern, ListParWithRandom] = matchListPar[F]

  def time[A](msg: String)(block: F[A]) =
    Stopwatch.time(log)(msg)(block)

  def msTime[A](block: F[A]): F[A] =
    Stopwatch.msTime(shortLog)(block)

  def nsTime[A](block: F[A]): F[(A, Long)] =
    Stopwatch.nsTimeToLong(block)

  def hash(hex: String) = Blake2b256Hash.fromHex(hex)
}
