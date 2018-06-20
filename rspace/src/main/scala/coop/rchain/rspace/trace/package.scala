package coop.rchain.rspace

import coop.rchain.rspace.internal.MultisetMultiMap

import scala.collection.immutable

package object trace {

  type Log = immutable.Seq[Event]

  type ReplayData = MultisetMultiMap[IOEvent, COMM]
}
