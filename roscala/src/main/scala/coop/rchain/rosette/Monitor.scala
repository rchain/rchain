package coop.rchain.rosette

case class Monitor(meta: Ob,
                   parent: Ob,
                   id: Ob,
                   timer: Timer,
                   opcodeCounts: Map[Op, Long],
                   obCounts: Long,
                   tracing: Boolean)
    extends Ob {
  def reset(): Unit                  = {}
  def start(): Unit                  = timer.start()
  def stop(): Unit                   = timer.stop()
  def printStats(file: String): Unit = {}
}

object Monitor {
  def apply(id: Ob): Monitor =
    new Monitor(meta = null, parent = null, id, Timer(), Map(), 0, tracing = false)
}
