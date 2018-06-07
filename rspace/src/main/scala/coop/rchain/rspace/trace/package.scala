package coop.rchain.rspace

package object trace {

  type Log = Seq[Event]

  type ReplayData = Map[IOEvent, COMM]
}
