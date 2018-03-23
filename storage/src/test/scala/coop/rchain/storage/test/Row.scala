package coop.rchain.storage.test

import coop.rchain.storage.{Datum, WaitingContinuation}

case class Row[P, A, K](data: Option[List[Datum[A]]], wks: Option[List[WaitingContinuation[P, K]]])
