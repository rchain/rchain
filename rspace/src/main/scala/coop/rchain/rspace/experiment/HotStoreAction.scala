package coop.rchain.rspace
package experiment

import internal.{Datum, WaitingContinuation}

sealed trait HotStoreAction

sealed trait InsertAction                                          extends HotStoreAction
final case class InsertData[C, A](channel: C, data: Seq[Datum[A]]) extends InsertAction
final case class InsertJoins[C](channel: C, joins: Seq[Seq[C]])    extends InsertAction
final case class InsertContinuations[C, P, K](
    channels: Seq[C],
    continuations: Seq[WaitingContinuation[P, K]]
) extends InsertAction

sealed trait DeleteAction                   extends HotStoreAction
final case class DeleteData[C](channel: C)  extends DeleteAction
final case class DeleteJoins[C](channel: C) extends DeleteAction
final case class DeleteContinuations[C](
    channels: Seq[C]
) extends DeleteAction
