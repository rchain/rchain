package coop.rchain.rholang.interpreter

class IndexMapChain[T](val chain: Seq[DebruijnIndexMap[T]]) {
  def this() = this(Seq(DebruijnIndexMap[T]()))

  def newBinding(binding: (String, T, Int, Int)): IndexMapChain[T] =
    IndexMapChain(chain.head.newBinding(binding) +: chain.tail)

  def newBindings(bindings: List[(String, T, Int, Int)]): IndexMapChain[T] =
    IndexMapChain(chain.head.newBindings(bindings) +: chain.tail)

  def absorbFree(binders: DebruijnLevelMap[T]): (IndexMapChain[T], List[(String, Int, Int)]) = {
    val (headAbsorbed, shadowed) = chain.head.absorbFree(binders)
    (IndexMapChain(headAbsorbed +: chain.tail), shadowed)
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

  def pushDown(): IndexMapChain[T] =
    IndexMapChain(DebruijnIndexMap[T]() +: chain)

  def getDeep(varName: String): Option[((Int, T, Int, Int), Int)] = {
    def getDeepLoop(varName: String,
                    depth: Int,
                    chain: Seq[DebruijnIndexMap[T]]): Option[((Int, T, Int, Int), Int)] =
      chain match {
        case head +: tail =>
          head.get(varName) match {
            case Some(result) => Some((result, depth))
            case None         => getDeepLoop(varName, depth + 1, tail)
          }
        case Nil => None
      }
    getDeepLoop(varName, 1, chain.tail)
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
  def apply[T](chain: Seq[DebruijnIndexMap[T]]): IndexMapChain[T] =
    new IndexMapChain[T](chain)

  def apply[T](): IndexMapChain[T] =
    new IndexMapChain[T]()

  def unapply[T](ic: IndexMapChain[T]): Option[Seq[DebruijnIndexMap[T]]] =
    Some(ic.chain)
}
