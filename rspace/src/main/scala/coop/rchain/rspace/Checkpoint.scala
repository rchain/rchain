package coop.rchain.rspace

case class Checkpoint(root: Blake2b256Hash, log: trace.Log)
