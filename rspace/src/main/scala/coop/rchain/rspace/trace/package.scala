package coop.rchain.rspace

import coop.rchain.rspace.internal.MultisetMultiMap

package object trace {

  type Log = Vector[Event]

  type ReplayData = MultisetMultiMap[IOEvent, COMM]
}
