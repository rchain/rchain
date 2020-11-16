package coop.rchain.rholang.interpreter.compiler

final case class IndexMapChain[T](chain: Vector[DeBruijnIndexMap[T]]) {

  def get(name: String): Option[IndexContext[T]] = chain.head.get(name)

  def find(name: String): Option[(IndexContext[T], Int)] = {
    import cats.implicits.catsStdInstancesForVector
    import cats.Foldable.ops.toAllFoldableOps
    chain.zipWithIndex.collectFirstSome {
      case (indexMap, depth) =>
        indexMap.get(name).map((_, depth))
    }
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
