package coop.rchain.rspace

import coop.rchain.rspace.internal.{Datum, WaitingContinuation}

sealed trait HotStoreAction
final case class InsertData[C, A](key: Blake2b256Hash, channel: C, data: Seq[Datum[A]])
    extends HotStoreAction
final case class InsertJoins[C](key: Blake2b256Hash, channel: C, joins: Seq[Seq[C]])
    extends HotStoreAction
final case class InsertContinuations[C, P, K](
    key: Blake2b256Hash,
    channels: Seq[C],
    continuations: Seq[WaitingContinuation[P, K]]
) extends HotStoreAction

final case class DeleteData[C, A](key: Blake2b256Hash, channel: C) extends HotStoreAction
final case class DeleteJoins[C](key: Blake2b256Hash, channel: C)   extends HotStoreAction
final case class DeleteContinuations[C, P, K](
    key: Blake2b256Hash,
    channels: Seq[C]
) extends HotStoreAction
