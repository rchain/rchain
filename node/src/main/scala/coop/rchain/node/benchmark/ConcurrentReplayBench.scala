package coop.rchain.node.benchmark

import cats.Parallel
import cats.effect.concurrent.Ref
import cats.effect.{Blocker, Concurrent, ContextShift, Resource}
import cats.syntax.all._
import coop.rchain.casper.genesis.Genesis
import coop.rchain.casper.genesis.contracts.Validator
import coop.rchain.casper.protocol.{CommEvent, ConsumeEvent, ProduceEvent}
import coop.rchain.casper.util.rholang.RuntimeManager
import coop.rchain.crypto.signatures.Secp256k1
import coop.rchain.metrics.Metrics.Source
import coop.rchain.metrics.{Metrics, Span}
import coop.rchain.node.benchmark.utils.GenesisParams.genesisParameters
import coop.rchain.node.benchmark.utils.LeaderfulSimulation.ValidatorWithPayments
import coop.rchain.node.benchmark.utils.{Payment, StateTransition, User}
import coop.rchain.rspace.syntax._
import coop.rchain.shared.syntax._
import coop.rchain.shared.{Log, Stopwatch, Time}
import coop.rchain.store.InMemoryStoreManager
import fs2.Stream
import io.circe.syntax._
import monix.execution.Scheduler

import java.nio.file.Path
import scala.collection.Seq
import scala.concurrent.duration.{FiniteDuration, NANOSECONDS}

/** Benchmark for concurrent state transitions. This is equivalent to concurrent block validation. */
object ConcurrentReplayBench {

  def go[F[_]: Concurrent: Parallel: Time: Log: ContextShift](
      stateTransitionsMax: Int,
      dataDir: Path
  )(implicit scheduler: Scheduler): F[Unit] = {
    implicit val m = new Metrics.MetricsNOP[F]

    // run everything in memory
    val rnodeStoreManager = new InMemoryStoreManager

    for {
      rSpaceStores <- rnodeStoreManager.rSpaceStores
      // extract all performance data gathered by Span trait usage across codebase
      statsRef <- Ref.of[F, Map[String, (Long, Int)]](Map.empty)
      profiler = new Span[F] {
        override def trace[A](source: Source)(block: F[A]): F[A] =
          for {
            v         <- Stopwatch.durationNano(block)
            (r, time) = v
            _ <- statsRef.update { s =>
                  val (currTimeAcc, currCallsAcc) = s.getOrElse(source, (0L, 0))
                  val newV                        = (currTimeAcc + time, currCallsAcc + 1)
                  s.updated(source, newV)
                }
          } yield r

        // do not need these one
        override def mark(name: String): F[Unit]                    = ().pure[F]
        override def withMarks[A](label: String)(block: F[A]): F[A] = block
      }
      runtimeManager <- {
        implicit val span = profiler
        RuntimeManager(rSpaceStores)
      }

      _     <- Log[F].info(s"Preparing genesis block...")
      users = User.random.take(stateTransitionsMax).toList
      validatorsKeys = (1 to stateTransitionsMax)
        .map(_ => Secp256k1.newKeyPair)
        .map { case (_, pk) => pk }
        .toList
      genesisVaults = users.map(_.pk)
      bondedValidators = validatorsKeys.zipWithIndex.map {
        case (v, i) => Validator(v, (2L * i.toLong + 1L))
      }
      genesis <- Genesis.createGenesisBlock(
                  runtimeManager,
                  genesisParameters(bondedValidators, genesisVaults)
                )
      _ <- Log[F].info(s"Genesis done.")

      r <- {
        implicit val rm = runtimeManager
        implicit val blocker =
          Blocker.liftExecutionContext(scala.concurrent.ExecutionContext.global)

        // first is a warm up for JVM
        (Stream(1, 1, 2, 3, 4, 5, 7) ++ Stream.range(10, stateTransitionsMax + 5, 5))
          .evalMap { networkSize =>
            val validators = validatorsKeys.take(networkSize)
            val payments   = Payment.random(users, 1, 10).take(networkSize).toList
            val transitions = validators
              .zip(payments)
              .map { case (v, p) => ValidatorWithPayments(v, Seq(p)) }

            val test = Stream
              .emits(transitions)
              .parEvalMapProcBounded { v =>
                StateTransition.make(
                  genesis.body.state.postStateHash,
                  v.validator,
                  0,
                  1,
                  v.payments.toList
                )
              }
              .compile
              .toList

            for {
              _               <- Log[F].info(s"Running ${transitions.size} concurrent STs.")
              r               <- Stopwatch.durationNano(test)
              (results, time) = r
              timeStr         = Stopwatch.showTime(FiniteDuration(time, NANOSECONDS))
              avgDeploysPerST = results.flatMap(_.processedDeploys).size.toFloat / results.size
              ps = results
                .flatMap(_.processedDeploys.flatMap(_.deployLog))
                .collect { case c: ProduceEvent => c }
                .size
              cs = results
                .flatMap(_.processedDeploys.flatMap(_.deployLog))
                .collect { case c: ConsumeEvent => c }
                .size
              comms = results
                .flatMap(_.processedDeploys.flatMap(_.deployLog))
                .collect { case c: CommEvent => c }
                .size
              psSys = results
                .flatMap(_.processedSysDeploys.flatMap(_.eventList))
                .collect { case c: ProduceEvent => c }
                .size
              csSys = results
                .flatMap(_.processedSysDeploys.flatMap(_.eventList))
                .collect { case c: ConsumeEvent => c }
                .size
              commsSys = results
                .flatMap(_.processedSysDeploys.flatMap(_.eventList))
                .collect { case c: CommEvent => c }
                .size
              cps   = comms.toFloat / time * 1e9
              stats <- statsRef.get
              logMsg = stats.toList
                .sortBy { case (_, (v, _)) => v }
                .reverse
                .foldLeft(
                  s"\nDONE: ${results.size} State transitions (avg $avgDeploysPerST TX per ST), " +
                    s"user events: $ps P, $cs C $comms COMM, " +
                    s"sys events : $psSys P, $csSys C $commsSys COMM, " +
                    s"time: ${timeStr}. COMMS per sec: ${cps}"
                ) {
                  case (acc, (metric, (totalTime, totalCalls))) =>
                    val timeS = totalTime.toFloat / 1e9
                    acc + f"\n$metric%60s: avg ${timeS / totalCalls}%.7f s, total $timeS%.7f s, calls ${totalCalls}, "
                }
              _ <- Log[F].info(logMsg)
              _ <- statsRef.set(Map.empty) //reset stats
              r = stats.map {
                case (metric, (total, qty)) =>
                  metric -> ("size" -> networkSize, "total" -> total, "calls" -> qty)
              }.asJson
            } yield r
          }
          .flatMap(v => Stream.fromIterator(v.show.getBytes.iterator))
          .through(fs2.io.file.writeAll(dataDir.resolve("out.json"), blocker))
          .compile
          .lastOrError
      }
    } yield ()
  }
}
