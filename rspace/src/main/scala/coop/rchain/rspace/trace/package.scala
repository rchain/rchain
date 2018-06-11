package coop.rchain.rspace

import scala.collection.immutable.Seq

package object trace {

  type Log = Seq[Event]

  type ReplayData = Map[IOEvent, COMM]
}
