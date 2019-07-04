package coop.rchain.rspace

final case class SoftCheckpoint[C, P, A, K](
    cacheSnapshot: Snapshot[C, P, A, K],
    log: trace.Log
)
final case class Checkpoint(root: Blake2b256Hash, log: trace.Log)
