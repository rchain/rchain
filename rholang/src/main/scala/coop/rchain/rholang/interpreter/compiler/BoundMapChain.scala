package coop.rchain.rholang.interpreter.compiler

import cats.syntax.all._

/**
  * A structure for keeping track of bound variables. Every time we recurse into a pattern, we push a fresh bound map
  * onto the top of the chain. This allows us to keep track of locally bound variables. For example, in the process
  *
  *   for(for(z <- @1){ Nil } <- @1){ Nil }
  *
  * we push a new bound map onto the chain when we recurse into
  *
  *   for(z <- @1){ Nil }
  *
  * which allows us to record information about the variable z.
  *
  * @param chain A vector of bound variable maps.
  * @tparam T The typing discipline we're enforcing.
  */
final case class BoundMapChain[T](chain: Vector[BoundMap[T]]) {

  def get(name: String): Option[BoundContext[T]] = chain.head.get(name)

  def find(name: String): Option[(BoundContext[T], Int)] =
    chain.zipWithIndex.collectFirstSome {
      case (indexMap, depth) =>
        indexMap.get(name).map((_, depth))
    }

  def put(binding: IdContext[T]): BoundMapChain[T] =
    BoundMapChain(chain.updated(0, chain(0).put(binding)))

  def put(bindings: List[IdContext[T]]): BoundMapChain[T] =
    BoundMapChain(chain.updated(0, chain(0).put(bindings)))

  def absorbFree(binders: FreeMap[T]): BoundMapChain[T] =
    BoundMapChain(chain.updated(0, chain(0).absorbFree(binders)))

  def push: BoundMapChain[T] = BoundMapChain(BoundMap.empty[T] +: chain)

  def count: Int = chain.head.count

  def depth: Int = chain.size - 1

}

object BoundMapChain {
  def empty[T]: BoundMapChain[T] = BoundMapChain(Vector(BoundMap.empty))
}
