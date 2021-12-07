package coop.rchain.rspace

import coop.rchain.rspace.hashing.Blake2b256Hash
import coop.rchain.rspace.trace.Produce

final case class SoftCheckpoint[C, P, A, K](
    cacheSnapshot: HotStoreState[C, P, A, K],
    log: trace.Log,
    produceCounter: Map[Produce, Int]
)
final case class Checkpoint(root: Blake2b256Hash, log: trace.Log)
