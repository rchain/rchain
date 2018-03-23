package coop.rchain.storage

final case class Datum[A](a: A, persist: Boolean)

private[storage] final case class DataCandidate[C, A](channel: C, datum: Datum[A], datumIndex: Int)

final case class WaitingContinuation[P, K](patterns: List[P], continuation: K, persist: Boolean)

private[storage] final case class ProduceCandidate[C, P, A, K](
    channels: List[C],
    continuation: WaitingContinuation[P, K],
    continuationIndex: Int,
    dataCandidates: List[DataCandidate[C, A]])
