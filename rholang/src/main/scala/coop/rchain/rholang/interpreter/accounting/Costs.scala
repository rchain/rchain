package coop.rchain.rholang.interpreter.accounting

import com.google.protobuf.ByteString
import coop.rchain.models.{Par, ProtoM, StacksafeMessage}

//TODO(mateusz.gorski): Adjust the costs of operations
final case class Cost(value: Long) extends AnyVal {
  def *(base: Int): Cost   = Cost(value * base)
  def +(other: Cost): Cost = Cost(value + other.value)
  def -(other: Cost): Cost = Cost(value - other.value)
}

object Cost {
  def apply[A](term: A)(implicit chargeable: Chargeable[A]): Cost =
    Cost(chargeable.cost(term))
  def apply(value: Int): Cost = Cost(value.toLong)
}

trait Costs {

  final val SUM_COST: Cost         = Cost(3)
  final val SUBTRACTION_COST: Cost = Cost(3)

  def equalityCheckCost[T <: StacksafeMessage[_], P <: StacksafeMessage[_]](x: T, y: P): Cost =
    Cost(
      scala.math.min(ProtoM.serializedSize(x).value, ProtoM.serializedSize(y).value)
    )

  final val BOOLEAN_AND_COST = Cost(2)
  final val BOOLEAN_OR_COST  = Cost(2)

  final val COMPARISON_COST: Cost     = Cost(3)
  final val MULTIPLICATION_COST: Cost = Cost(9)
  final val DIVISION_COST: Cost       = Cost(9)

  // operations on collections
  // source: https://docs.scala-lang.org/overviews/collections/performance-characteristics.html
  final val LOOKUP_COST = Cost(3) // map/set lookup is eC
  final val REMOVE_COST = Cost(3) // map/set remove is eC
  final val ADD_COST    = Cost(3) // map/set add is eC

  // decoding to bytes is linear with respect to the length of the string
  def hexToBytesCost(str: String): Cost = Cost(str.size)

  // Both Set#remove and Map#remove have complexity of eC
  def diffCost(numElements: Int): Cost = REMOVE_COST * numElements

  // Both Set#add and Map#add have complexity of eC
  def unionCost(numElements: Int): Cost = ADD_COST * numElements

  // GByteArray uses ByteString internally which in turn are implemented using
  // data structure called Rope for which append operation is O(logN)
  def byteArrayAppendCost(left: ByteString): Cost = Cost(math.log10(left.size().toDouble).toInt)
  // According to scala doc Vector#append is eC so it's n*eC.
  def listAppendCost(right: Vector[Par]): Cost = Cost(right.size)
  // String append creates a char[] of size n + m and then copies all elements to it.
  def stringAppendCost(n: Int, m: Int): Cost = Cost(n + m)

  // To interpolate we traverse whole base string and for each placeholder
  // we look for matching key in the interpolation map
  def interpolateCost(strLength: Int, mapSize: Int): Cost = Cost(strLength * mapSize)

  // serializing any Par into a Array[Byte]:
  // + allocates byte array of the same size as `serializedSize`
  // + then it copies all elements of the Par
  def toByteArrayCost[T <: StacksafeMessage[_]](a: T): Cost =
    Cost(ProtoM.serializedSize(a).value)
  //TODO: adjust the cost of size method
  def sizeMethodCost(size: Int): Cost = Cost(size)
  // slice(from, to) needs to drop `from` elements and then append `to - from` elements
  // we charge proportionally to `to` and fail if the method call is incorrect, for example
  // if underlying string is shorter then the `to` value.
  def sliceCost(to: Int): Cost = Cost(to)

  final val NTH_METHOD_CALL_COST: Cost = Cost(10)

  final val KEYS_METHOD_COST: Cost = Cost(10)

  final val LENGTH_METHOD_COST = Cost(10)
  final val METHOD_CALL_COST   = Cost(10)
  final val OP_CALL_COST       = Cost(10)
  final val VAR_EVAL_COST      = Cost(10)
  final val SEND_EVAL_COST     = Cost(11)
  final val RECEIVE_EVAL_COST  = Cost(11)
  final val CHANNEL_EVAL_COST  = Cost(11)

  // The idea is that evaluation of `new x1, x2, …, xn in { }` should be charged depending
  // on the # of bindings and constant cost of evaluating `new … in  { … }` construct
  final val NEW_BINDING_COST  = Cost(2)
  final val NEW_EVAL_COST     = Cost(10)
  def newBindingsCost(n: Int) = (NEW_BINDING_COST * n) + NEW_EVAL_COST

  final val MATCH_EVAL_COST = Cost(12)

  implicit def toStorageCostOps[A <: StacksafeMessage[_]](a: Seq[A]) = new StorageCostOps(a: _*)

  implicit def toStorageCostOps[A <: StacksafeMessage[_]](a: A) = new StorageCostOps(a)

  class StorageCostOps[A <: StacksafeMessage[_]](a: A*) {
    def storageCost: Cost = Cost(a.map(a => ProtoM.serializedSize(a).value).sum)
  }

  // Even though we use Long for phlo limit we can't use Long.MaxValue here.
  // This is because in tests, when  we refund deployment we add phlos to the deployment's limit
  // which may result in overflow when we start with maximum value.
  // In normal scenario this will never happen because user would need to provide equivalent of Long.MaxValue
  // in REVs and this is more than there is REVs available.
  final val MAX_VALUE = Integer.MAX_VALUE
}
