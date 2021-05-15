package coop.rchain.rspace

import coop.rchain.rspace.internal.{Datum, WaitingContinuation}

sealed trait HotStoreAction

sealed trait InsertAction                                                extends HotStoreAction
final case class InsertData[A](channel: Channel, data: Seq[Datum[A]])    extends InsertAction
final case class InsertJoins(channel: Channel, joins: Seq[Seq[Channel]]) extends InsertAction
final case class InsertContinuations[P, K](
    channels: Seq[Channel],
    continuations: Seq[WaitingContinuation[P, K]]
) extends InsertAction

sealed trait DeleteAction                      extends HotStoreAction
final case class DeleteData(channel: Channel)  extends DeleteAction
final case class DeleteJoins(channel: Channel) extends DeleteAction
final case class DeleteContinuations(
    channels: Seq[Channel]
) extends DeleteAction
