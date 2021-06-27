package coop.rchain.node.runtime

import coop.rchain.node.diagnostics.Trace
import coop.rchain.node.diagnostics.Trace.TraceId

final case class NodeCallCtx(trace: TraceId) {
  def next: NodeCallCtx = this.copy(trace = Trace.next)
}

object NodeCallCtx {
  def init: NodeCallCtx = NodeCallCtx(Trace.next)
}
