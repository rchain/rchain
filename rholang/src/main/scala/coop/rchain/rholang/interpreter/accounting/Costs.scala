package coop.rchain.rholang.interpreter.accounting

import coop.rchain.models.Par
import scalapb.{GeneratedMessage, GeneratedMessageCompanion, Message}

//TODO: Adjust the costs of operations
trait Costs {

  type Cost = BigInt

  final val BOOLEAN_COST: Cost = 1
  final val INT_COST: Cost     = 2

  final val SUM_COST: Cost         = 3
  final val SUBTRACTION_COST: Cost = 3

  def equalityCheckCost(x: Par, y: Par) = scala.math.min(x.serializedSize, y.serializedSize)

  final val BOOLEAN_AND_COST = 2
  final val BOOLEAN_OR_COST  = 2

  final val COMPARISON_COST: Cost     = 3
  final val MULTIPLICATION_COST: Cost = 9
  final val DIVISION_COST: Cost       = 9

  // operations on collections
  // source: https://docs.scala-lang.org/overviews/collections/performance-characteristics.html
  final val LOOKUP_COST = 3 // map/set lookup is eC
  final val REMOVE_COST = 3 // map/set remove is eC
  final val ADD_COST    = 3 // map/set add is eC

  // decoding to bytes is linear with respect to the length of the string
  def hexToByteCost(str: String): Cost = str.size

  // serializing any Par into a Array[Byte]:
  // + allocates byte array of the same size as `serializedSize`
  // + then it copies all elements of the Par
  def toByteArrayCost[T <: GeneratedMessage with Message[T]](a: T)(
      implicit comp: GeneratedMessageCompanion[T]): Cost =
    a.serializedSize

  //TODO: adjust the cost of the nth method call.
  def nthMethodCost(nth: Int): Cost = nth

  final val METHOD_CALL_COST  = 10
  final val VAR_EVAL_COST     = 10
  final val SEND_EVAL_COST    = 11
  final val RECEIVE_EVAL_COST = 11
  final val CHANNEL_EVAL_COST = 11

  // The idea is that evaluation of `new x1, x2, …, xn in { }` should be charged depending
  // on the # of bindings and constant cost of evaluating `new … in  { … }` construct
  final val NEW_BINDING_COST  = 2
  final val NEW_EVAL_COST     = 10
  def newBindingsCost(n: Int) = n * NEW_BINDING_COST + NEW_EVAL_COST

  final val MATCH_EVAL_COST = 12

  implicit def toStorageCostOps[A <: GeneratedMessage with Message[A]](a: Seq[A])(
      implicit gm: GeneratedMessageCompanion[A]) = new StorageCostOps(a: _*)(gm)

  implicit def toStorageCostOps[A <: GeneratedMessage with Message[A]](a: A)(
      implicit gm: GeneratedMessageCompanion[A]) = new StorageCostOps(a)(gm)

  class StorageCostOps[A <: GeneratedMessage with Message[A]](a: A*)(
      gm: GeneratedMessageCompanion[A]) {
    def storageCost: Cost = a.map(a => gm.toByteArray(a).size).sum
  }
}
