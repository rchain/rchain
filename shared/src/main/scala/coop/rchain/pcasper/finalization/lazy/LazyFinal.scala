package coop.rchain.pcasper.finalization.`lazy`
import coop.rchain.pcasper.finalization._

/**
  * Lazy finality.
  * @param complete   Complete fringe going across all senders of a shard
  * @param provisional Fringes inside partitions detected.
  *                    Partitions cannot intersect, which is the property of Lazy Casper,
  *                    so its enough to store just the last [[Fringe]] of a partition.
  */
final case class LazyFinal[M, S](
    override val complete: Fringe[M, S],
    override val provisional: Fringe[M, S]
) extends Final[M, S] { override type Provisional = Fringe[M, S] }
