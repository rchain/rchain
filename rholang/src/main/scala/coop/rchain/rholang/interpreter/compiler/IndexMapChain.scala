package coop.rchain.rholang.interpreter.compiler

import cats.syntax.all._

/**
  *
  * A structure for keeping track of bound variables. Every time we go under a binding construct
  * (e.g. match or receive), we add a fresh index map to the top of the chain. For a language like
  * Java, each index map would represent a method's local variables.
  *
  * @param chain A list of bound variable maps.
  * @tparam T The typing discipline we're enforcing.
  */
final case class IndexMapChain[T](chain: Vector[DeBruijnIndexMap[T]]) {

  def get(name: String): Option[IndexContext[T]] = chain.head.get(name)

  def find(name: String): Option[(IndexContext[T], Int)] =
    chain.zipWithIndex.collectFirstSome {
      case (indexMap, depth) =>
        indexMap.get(name).map((_, depth))
    }

  def put(binding: IdContext[T]): IndexMapChain[T] =
    IndexMapChain(chain.updated(0, chain(0).put(binding)))

  def put(bindings: List[IdContext[T]]): IndexMapChain[T] =
    IndexMapChain(chain.updated(0, chain(0).put(bindings)))

  def absorbFree(binders: DeBruijnLevelMap[T]): IndexMapChain[T] =
    IndexMapChain(chain.updated(0, chain(0).absorbFree(binders)))

  def push: IndexMapChain[T] = IndexMapChain(DeBruijnIndexMap.empty[T] +: chain)

  def count: Int = chain.head.count

  def depth: Int = chain.size - 1

}

object IndexMapChain {
  def empty[T]: IndexMapChain[T] = IndexMapChain(Vector(DeBruijnIndexMap.empty))
}
