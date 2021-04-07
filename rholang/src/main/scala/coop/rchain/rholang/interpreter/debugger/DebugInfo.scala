package coop.rchain.rholang.interpreter.debugger

import coop.rchain.models.New

import scala.collection.concurrent.TrieMap

final case class DebugInfo(normMap: TrieMap[New, Seq[String]])

object DebugInfo {
  def apply(): DebugInfo = DebugInfo(normMap = TrieMap[New, Seq[String]]())
}
