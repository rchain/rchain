package coop.rchain.rholang.interpreter.accounting

import coop.rchain.shared.NumericOps
import scalapb.{GeneratedMessage, GeneratedMessageCompanion, Message}

//TODO(mateusz.gorski): Adjust the costs of operations
final case class Cost(value: BigInt) extends AnyVal {
  def *(other: Cost): Cost = Cost(value * other.value)
  def *(other: Int): Cost  = Cost(value * other)
  def +(other: Cost): Cost = Cost(value + other.value)
}

object Cost {
  implicit val costNumeric: Numeric[Cost] =
    NumericOps.by[Cost, BigInt](_.value, Cost(_))
}

trait Costs {

  final val SUM_COST: Cost         = Cost(3)
  final val SUBTRACTION_COST: Cost = Cost(3)

  def equalityCheckCost[T <: GeneratedMessage with Message[T],
                        P <: GeneratedMessage with Message[P]](x: T, y: P): Cost =
    Cost(scala.math.min(x.serializedSize, y.serializedSize))

  final val BOOLEAN_AND_COST = Cost(2)
  final val BOOLEAN_OR_COST  = Cost(2)

  final val COMPARISON_COST: Cost     = Cost(3)
  final val MULTIPLICATION_COST: Cost = Cost(9)
  final val DIVISION_COST: Cost       = Cost(9)

  final val STRING_APPEND_COST = Cost(3)

  // operations on collections
  // source: https://docs.scala-lang.org/overviews/collections/performance-characteristics.html
  final val LOOKUP_COST = Cost(3) // map/set lookup is eC
  final val REMOVE_COST = Cost(3) // map/set remove is eC
  final val ADD_COST    = Cost(3) // map/set add is eC

  final val PREPEND_COST = Cost(2) // list prepend is C

  // decoding to bytes is linear with respect to the length of the string
  def hexToByteCost(str: String): Cost = Cost(str.size)

  // serializing any Par into a Array[Byte]:
  // + allocates byte array of the same size as `serializedSize`
  // + then it copies all elements of the Par
  def toByteArrayCost[T <: GeneratedMessage with Message[T]](a: T)(
      implicit comp: GeneratedMessageCompanion[T]): Cost =
    Cost(a.serializedSize)

  //TODO(mateusz.gorski): adjust the cost of the nth method call.
  def nthMethodCost(nth: Int): Cost = Cost(nth)

  final val METHOD_CALL_COST  = Cost(10)
  final val OP_CALL_COST      = Cost(10)
  final val VAR_EVAL_COST     = Cost(10)
  final val SEND_EVAL_COST    = Cost(11)
  final val RECEIVE_EVAL_COST = Cost(11)
  final val CHANNEL_EVAL_COST = Cost(11)

  // The idea is that evaluation of `new x1, x2, …, xn in { }` should be charged depending
  // on the # of bindings and constant cost of evaluating `new … in  { … }` construct
  final val NEW_BINDING_COST  = Cost(2)
  final val NEW_EVAL_COST     = Cost(10)
  def newBindingsCost(n: Int) = Cost(n) * NEW_BINDING_COST + NEW_EVAL_COST

  final val MATCH_EVAL_COST = Cost(12)

  implicit def toStorageCostOps[A <: GeneratedMessage with Message[A]](a: Seq[A])(
      implicit gm: GeneratedMessageCompanion[A]) = new StorageCostOps(a: _*)(gm)

  implicit def toStorageCostOps[A <: GeneratedMessage with Message[A]](a: A)(
      implicit gm: GeneratedMessageCompanion[A]) = new StorageCostOps(a)(gm)

  class StorageCostOps[A <: GeneratedMessage with Message[A]](a: A*)(
      gm: GeneratedMessageCompanion[A]) {
    def storageCost: Cost = Cost(a.map(a => gm.toByteArray(a).size).sum)
  }
}
