package coop.rchain.rspace
package experiment

import internal.MultisetMultiMap

package object trace {

  type Log = Seq[Event]

  type ReplayData = MultisetMultiMap[IOEvent, COMM]
}
