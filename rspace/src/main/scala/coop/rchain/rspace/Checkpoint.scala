package coop.rchain.rspace

import coop.rchain.rspace.trace.Produce

final case class SoftCheckpoint[C, P, A, K](
    cacheSnapshot: Snapshot[C, P, A, K],
    log: trace.Log,
    produceCounter: Map[Produce, Int]
)
final case class Checkpoint(
    root: Blake2b256Hash,
    log: trace.Log,
    changes: Seq[HotStoreAction] = Seq.empty
)
