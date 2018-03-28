package coop.rchain.rspace

object internal {

  final case class Datum[A](a: A, persist: Boolean)

  final case class DataCandidate[C, A](channel: C, datum: Datum[A], datumIndex: Int)

  final case class WaitingContinuation[P, K](patterns: List[P], continuation: K, persist: Boolean)

  final case class ProduceCandidate[C, P, A, K](channels: List[C],
                                                continuation: WaitingContinuation[P, K],
                                                continuationIndex: Int,
                                                dataCandidates: List[DataCandidate[C, A]])

  case class Row[P, A, K](data: List[Datum[A]], wks: List[WaitingContinuation[P, K]])
}
