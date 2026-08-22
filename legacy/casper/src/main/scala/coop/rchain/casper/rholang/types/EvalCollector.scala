package coop.rchain.casper.rholang.types

import coop.rchain.casper.protocol.Event
import coop.rchain.models.Par

final case class EvalCollector(eventLog: Vector[Event], mergeableChannels: Set[Par]) {
  def addEventLog(log: Seq[Event]): EvalCollector =
    copy(eventLog = eventLog ++ log)

  def addMergeableChannels(mergeChs: Set[Par]): EvalCollector =
    copy(mergeableChannels = mergeableChannels ++ mergeChs)

  def add(log: Seq[Event], mergeChs: Set[Par]): EvalCollector =
    copy(eventLog ++ log, mergeableChannels ++ mergeChs)
}

object EvalCollector {
  def apply(): EvalCollector = EvalCollector(Vector(), Set())
}
