package coop.rchain.pcasper.finalization

trait Partitioner[F[_], M, S] {

  /**
    * Find partition on top of the base fringe.
    * @return first [[Level]] of a partition
    */
  def findPartition(base: Fringe[M, S]): F[Option[Fringe[M, S]]]
}
