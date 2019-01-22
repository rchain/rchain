package coop.rchain.rspace

final case class Checkpoint(root: Blake2b256Hash, log: trace.Log)
