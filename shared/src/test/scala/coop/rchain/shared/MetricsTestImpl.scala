package coop.rchain.shared
import cats.effect.Sync
import coop.rchain.metrics.Metrics

import scala.collection.mutable.{Map => MutableMap}

class MetricsTestImpl[F[_]: Sync] extends Metrics[F] {
  val counters: MutableMap[String, Long] = MutableMap.empty
  val samplers: MutableMap[String, Long] = MutableMap.empty
  val gauges: MutableMap[String, Long]   = MutableMap.empty
  final case class Record(value: Long, count: Long)
  val records: MutableMap[String, List[Record]] = MutableMap.empty

  private def incrementBy(name: String, delta: Long)(m: MutableMap[String, Long]): Unit = {
    val newValue = m.get(name).map(_ + delta).getOrElse(delta)
    m.update(name, newValue)
  }

  private def set[A](name: String, value: A)(m: MutableMap[String, A]): Unit =
    m.update(name, value)

  override def incrementCounter(name: String, delta: Long): F[Unit] =
    Sync[F].delay(incrementBy(name, delta)(counters))

  override def incrementSampler(name: String, delta: Long): F[Unit] =
    Sync[F].delay(incrementBy(name, delta)(samplers))

  // no idea how to implement this properly
  override def sample(name: String): F[Unit] = ???

  override def setGauge(name: String, value: Long): F[Unit] =
    Sync[F].delay(set(name, value)(gauges))
  override def incrementGauge(name: String, delta: Long): F[Unit] =
    Sync[F].delay(incrementBy(name, delta)(gauges))
  override def decrementGauge(name: String, delta: Long): F[Unit] =
    Sync[F].delay(incrementBy(name, -delta)(gauges))
  override def record(name: String, value: Long, count: Long): F[Unit] =
    Sync[F].delay {
      val record     = Record(value, count)
      val recordsSeq = records.get(name).map(s => record :: s).getOrElse(List(record))
      set(name, recordsSeq)(records)
    }
}
