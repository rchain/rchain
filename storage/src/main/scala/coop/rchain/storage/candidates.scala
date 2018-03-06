package coop.rchain.storage

final case class DataCandidate[C, A](channel: C, datum: A, datumIndex: Int)

final case class ProduceCandidate[C, A, K](channels: List[C],
                                           continuation: K,
                                           continuationIndex: Int,
                                           dataCandidates: List[DataCandidate[C, A]])
