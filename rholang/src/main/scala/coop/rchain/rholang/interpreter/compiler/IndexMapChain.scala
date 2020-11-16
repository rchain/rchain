package coop.rchain.rholang.interpreter.compiler

final case class IndexMapChain[T](chain: IndexedSeq[DeBruijnIndexMap[T]]) {

  def put(binding: (String, T, Int, Int)): IndexMapChain[T] =
    IndexMapChain(chain.updated(0, chain(0).put(binding)))

  def put(bindings: List[(String, T, Int, Int)]): IndexMapChain[T] =
    IndexMapChain(chain.updated(0, chain(0).put(bindings)))

  def absorbFree(binders: DeBruijnLevelMap[T]): (IndexMapChain[T], List[(String, Int, Int)]) = {
    val (headAbsorbed, shadowed) = chain.head.absorbFree(binders)
    (IndexMapChain(chain.updated(0, headAbsorbed)), shadowed)
  }

  def get(varName: String): Option[(Int, T, Int, Int)] = chain.head.get(varName)

  def count: Int =
    chain.head.count

  def depth: Int =
    chain.size - 1

  def pushDown(): IndexMapChain[T] =
    IndexMapChain(DeBruijnIndexMap.empty[T] +: chain)

  def getDeep(varName: String): Option[((Int, T, Int, Int), Int)] = {
    def getDeepLoop(varName: String, depth: Int): Option[((Int, T, Int, Int), Int)] =
      if (depth < chain.size) {
        chain(depth).get(varName) match {
          case Some(result) => Some((result, depth))
          case None         => getDeepLoop(varName, depth + 1)
        }
      } else {
        None
      }
    getDeepLoop(varName, 1)
  }
}

object IndexMapChain {
  def empty[T]: IndexMapChain[T] = IndexMapChain(IndexedSeq(DeBruijnIndexMap.empty))
}
