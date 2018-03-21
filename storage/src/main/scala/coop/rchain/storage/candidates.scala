package coop.rchain.storage

private[storage] final case class DataCandidate[C, A](channel: C, datum: A, datumIndex: Int)

private[storage] final case class ProduceCandidate[C, A, K](
    channels: List[C],
    continuation: K,
    continuationIndex: Int,
    dataCandidates: List[DataCandidate[C, A]])
