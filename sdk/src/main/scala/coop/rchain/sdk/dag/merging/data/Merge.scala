package coop.rchain.sdk.dag.merging.data

final case class Merge[S, D](base: S, toAccept: Set[D], toReject: Set[D])
