package coop.rchain.rspace

import coop.rchain.rspace.internal.{Datum, WaitingContinuation}

sealed trait HotStoreAction
final case class InsertData[C, A](channel: C, data: Seq[Datum[A]]) extends HotStoreAction
final case class InsertJoins[C](channel: C, joins: Seq[Seq[C]])    extends HotStoreAction
final case class InsertContinuations[C, P, K](
    channels: Seq[C],
    continuations: Seq[WaitingContinuation[P, K]]
) extends HotStoreAction

final case class DeleteData[C, A](channel: C) extends HotStoreAction
final case class DeleteJoins[C](channel: C)   extends HotStoreAction
final case class DeleteContinuations[C, P, K](
    channels: Seq[C]
) extends HotStoreAction
