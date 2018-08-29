package coop.rchain.rholang.interpreter

class IndexMapChain[T](val chain: IndexedSeq[DebruijnIndexMap[T]]) {
  def this() = this(IndexedSeq(DebruijnIndexMap[T]()))

  def newBinding(binding: (String, T, Int, Int)): IndexMapChain[T] =
    IndexMapChain(chain.updated(0, chain(0).newBinding(binding)))

  def newBindings(bindings: List[(String, T, Int, Int)]): IndexMapChain[T] =
    IndexMapChain(chain.updated(0, chain(0).newBindings(bindings)))

  def absorbFree(binders: DebruijnLevelMap[T]): (IndexMapChain[T], List[(String, Int, Int)]) = {
    val (headAbsorbed, shadowed) = chain.head.absorbFree(binders)
    (IndexMapChain(chain.updated(0, headAbsorbed)), shadowed)
  }

  def getBinding(varName: String): Option[T] =
    chain.head.getBinding(varName)
  def getLevel(varName: String): Option[Int] =
    chain.head.getLevel(varName)
  def get(varName: String): Option[(Int, T, Int, Int)] =
    chain.head.get(varName)
  def isEmpty(): Boolean =
    chain.head.isEmpty()

  def count: Int =
    chain.head.count

  def depth: Int =
    chain.size - 1

  def pushDown(): IndexMapChain[T] =
    IndexMapChain(DebruijnIndexMap[T]() +: chain)

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

  override def equals(that: Any): Boolean =
    that match {
      case that: IndexMapChain[T] =>
        chain == that.chain
      case _ => false
    }

  override def hashCode(): Int =
    chain.hashCode() * 13
}

object IndexMapChain {
  def apply[T](chain: IndexedSeq[DebruijnIndexMap[T]]): IndexMapChain[T] =
    new IndexMapChain[T](chain)

  def apply[T](): IndexMapChain[T] =
    new IndexMapChain[T]()

  def unapply[T](ic: IndexMapChain[T]): Option[IndexedSeq[DebruijnIndexMap[T]]] =
    Some(ic.chain)
}
