package coop.rchain.sdk.simulation.data

import cats.Monad
import cats.data.NonEmptyList
import coop.rchain.sdk.dag.data.{DagData, DagView}
import coop.rchain.sdk.dag.syntax._
import coop.rchain.sdk.finalization.data.Finalization.Covering
import cats.syntax.all._
import fs2.Stream

/**
  * Represents commands for defining finalization simulation.
  */
trait SimulationControl[F[_]] {
  def defineGroup(name: String, nodes: Set[Int]): F[Unit]

  def setSection(name: String): F[Unit]

  def run(sections: Seq[String]): F[Unit]
}

object SimulationControl {
  def apply[F[_]](implicit instance: SimulationControl[F]): instance.type = instance
}
