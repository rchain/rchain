package coop.rchain.rholang.interpreter

import cats.data.StateT
import coop.rchain.models.Par

import scala.collection.immutable.Stream

package object matcher {

  type FreeMap            = Map[Int, Par]
  type OptionalFreeMap[A] = StateT[Option, FreeMap, A]
  type NonDetFreeMap[A]   = StateT[Stream, FreeMap, A]

  def emptyMap: FreeMap = Map.empty[Int, Par]

}
