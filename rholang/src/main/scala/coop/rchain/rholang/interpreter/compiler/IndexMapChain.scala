package coop.rchain.rholang.interpreter.compiler

import cats.syntax.all._

final case class IndexMapChain[T](chain: Vector[DeBruijnIndexMap[T]]) {

  def put(binding: IdContext[T]): IndexMapChain[T] =
    IndexMapChain(chain.updated(0, chain(0).put(binding)))

  def put(bindings: List[IdContext[T]]): IndexMapChain[T] =
    IndexMapChain(chain.updated(0, chain(0).put(bindings)))

  def absorbFree(binders: DeBruijnLevelMap[T]): IndexMapChain[T] =
    IndexMapChain(chain.updated(0, chain.head.absorbFree(binders)))

  def get(varName: String): Option[IndexContext[T]] = chain.head.get(varName)

  def count: Int = chain.head.count

  def depth: Int = chain.size - 1

  def pushDown(): IndexMapChain[T] = IndexMapChain(DeBruijnIndexMap.empty[T] +: chain)

  def getDeep(varName: String): Option[(IndexContext[T], Int)] = {
    import cats.implicits.catsStdInstancesForVector
    chain.zipWithIndex.collectFirstSome {
      case (indexMap, depth) =>
        indexMap.get(varName).map((_, depth))
    }
  }
}

object IndexMapChain {
  def empty[T]: IndexMapChain[T] = IndexMapChain(Vector(DeBruijnIndexMap.empty))
}
