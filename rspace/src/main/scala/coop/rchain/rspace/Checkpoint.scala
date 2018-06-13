package coop.rchain.rspace

case class Checkpoint(root: Option[Blake2b256Hash], log: trace.Log)
