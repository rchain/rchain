package coop.rchain.rholang.interpreter.accounting

import com.google.protobuf.ByteString
import coop.rchain.models.TaggedContinuation.TaggedCont.ParBody
import coop.rchain.models.{
  BindPattern,
  ListParWithRandom,
  PCost,
  Par,
  ParWithRandom,
  ProtoM,
  StacksafeMessage,
  TaggedContinuation
}
import coop.rchain.rspace.hashing.Blake2b256Hash

//TODO(mateusz.gorski): Adjust the costs of operations
final case class Cost(value: Long, operation: String) {
  def *(base: Int): Cost   = Cost(value * base, s"($operation * $base)")
  def +(other: Cost): Cost = Cost(value + other.value, "")
  def -(other: Cost): Cost = Cost(value - other.value, "")
}

object Cost {
  def apply[A](term: A)(implicit chargeable: Chargeable[A]): Cost =
    Cost(chargeable.cost(term), "")
  def apply[A](term: A, operation: String)(implicit chargeable: Chargeable[A]): Cost =
    Cost(chargeable.cost(term), operation)
  def apply(value: Int, operation: String): Cost = Cost(value.toLong, operation)
  def apply(cost: Cost, operation: String): Cost = Cost(cost.value, operation)
  // TODO remove the single-arg constructors and provide an operation description in all callsites
  def apply(value: Long): Cost = Cost(value, "")

  def toProto(c: Cost): PCost = PCost(c.value)

  // FIXME: remove this
  val UNSAFE_MAX = Cost(Integer.MAX_VALUE)
}

trait Costs {

  final val SUM_COST: Cost         = Cost(3, "sum")
  final val SUBTRACTION_COST: Cost = Cost(3, "subtraction")

  def equalityCheckCost[T <: StacksafeMessage[_], P <: StacksafeMessage[_]](x: T, y: P): Cost =
    Cost(
      scala.math.min(ProtoM.serializedSize(x).value, ProtoM.serializedSize(y).value),
      "equality check"
    )

  final val BOOLEAN_AND_COST = Cost(2, "boolean and")
  final val BOOLEAN_OR_COST  = Cost(2, "boolean or")

  final val COMPARISON_COST: Cost     = Cost(3, "comparison")
  final val MULTIPLICATION_COST: Cost = Cost(9, "multiplication")
  final val DIVISION_COST: Cost       = Cost(9, "division")
  final val MODULO_COST: Cost         = Cost(9, "modulo")

  // operations on collections
  // source: https://docs.scala-lang.org/overviews/collections/performance-characteristics.html
  final val LOOKUP_COST = Cost(3, "lookup")   // map/set lookup is eC
  final val REMOVE_COST = Cost(3, "removal")  // map/set remove is eC
  final val ADD_COST    = Cost(3, "addition") // map/set add is eC

  // decoding to bytes is linear with respect to the length of the string
  def hexToBytesCost(str: String): Cost = Cost(str.size, "hex to bytes")

  // encoding to hex is linear with respect to the length of the byte array
  def bytesToHexCost(bytes: Array[Byte]): Cost = Cost(bytes.size, "bytes to hex")

  // Both Set#remove and Map#remove have complexity of eC
  def diffCost(numElements: Int): Cost =
    Cost(REMOVE_COST * numElements, s"$numElements elements diff cost")

  // Both Set#add and Map#add have complexity of eC
  def unionCost(numElements: Int): Cost = Cost(ADD_COST * numElements, s"$numElements union cost")

  // GByteArray uses ByteString internally which in turn are implemented using
  // data structure called Rope for which append operation is O(logN)
  def byteArrayAppendCost(left: ByteString): Cost =
    Cost(math.log10(left.size().toDouble).toInt, "byte array append")
  // According to scala doc Vector#append is eC so it's n*eC.
  def listAppendCost(right: Vector[Par]): Cost = Cost(right.size, "list append")
  // String append creates a char[] of size n + m and then copies all elements to it.
  def stringAppendCost(n: Int, m: Int): Cost = Cost(n + m, "string append")

  // To interpolate we traverse whole base string and for each placeholder
  // we look for matching key in the interpolation map
  def interpolateCost(strLength: Int, mapSize: Int): Cost = Cost(strLength * mapSize, "interpolate")

  // serializing any Par into a Array[Byte]:
  // + allocates byte array of the same size as `serializedSize`
  // + then it copies all elements of the Par
  def toByteArrayCost[T <: StacksafeMessage[_]](a: T): Cost =
    Cost(ProtoM.serializedSize(a).value, "to byte array")

  // Converting rholang BigInt, String or ByteArray into a GInt.
  // The cost is estimated as the number of bytes in converted data.
  def toIntCost(bi: BigInt): Cost  = Cost(bigIntSize(bi), "bigint to int")
  def toIntCost(str: String): Cost = Cost(str.length, "string to int")

  // TODO: Adjust the BigInt operation cost
  // Converting rholang Int, String or ByteArray into a GBigInt.
  // The cost is estimated as the number of bytes in converted data.
  final val INT_TO_BIGINT_COST: Cost  = Cost(8, "int to bigint")
  def toBigIntCost(str: String): Cost = Cost(str.length, "string to bigint")

  // The number of bytes in the new array is used
  def bigIntNegation(bi: BigInt): Cost = Cost(bigIntSize(bi), "bigint negation")

  // Сompared byte by byte
  def bigIntComparison(left: BigInt, right: BigInt): Cost =
    Cost(scala.math.min(bigIntSize(left), bigIntSize(right)), "bigint comparison")

  // For storing result need to copy N+1 bytes
  def bigIntSum(left: BigInt, right: BigInt): Cost =
    Cost(scala.math.max(bigIntSize(left), bigIntSize(right)) + 1, "bigint sum")

  // For storing result need to copy N+1 bytes
  def bigIntSubtraction(left: BigInt, right: BigInt): Cost =
    Cost(scala.math.max(bigIntSize(left), bigIntSize(right)) + 1, "bigint subtraction")

  // Assumed to be used long multiplication
  def bigIntMultiplication(left: BigInt, right: BigInt): Cost =
    Cost(bigIntSize(left) * bigIntSize(right), "bigint multiplication")

  // Assumed to be used long division
  def bigIntDivision(left: BigInt, right: BigInt): Cost =
    Cost(bigIntSize(left) * bigIntSize(right), "bigint division")

  // Assumed to be used long division
  def bigIntModulo(left: BigInt, right: BigInt): Cost =
    Cost(bigIntSize(left) * bigIntSize(right), "bigint modulo")

  // Calculate number of bytes needed to storyng BigInt value
  def bigIntSize(bi: BigInt): Int = bi.bitLength / 8 + 1

  //TODO: adjust the cost of size method
  def sizeMethodCost(size: Int): Cost = Cost(size, "size")
  // slice(from, to) needs to drop `from` elements and then append `to - from` elements
  // we charge proportionally to `to` and fail if the method call is incorrect, for example
  // if underlying string is shorter then the `to` value.
  def sliceCost(to: Int): Cost = Cost(to, "slice")

  def takeCost(to: Int): Cost = Cost(to, "take")

  def toListCost(size: Int): Cost = Cost(size, "toList")

  def parsingCost(term: String): Cost = Cost(term.getBytes.size, "parsing")

  final val NTH_METHOD_CALL_COST: Cost = Cost(10, "nth method call")

  final val KEYS_METHOD_COST: Cost = Cost(10, "keys method")

  final val LENGTH_METHOD_COST = Cost(10, "length method")
  final val METHOD_CALL_COST   = Cost(10, "method call")
  final val OP_CALL_COST       = Cost(10, "op call")
  final val VAR_EVAL_COST      = Cost(10, "var eval")
  final val SEND_EVAL_COST     = Cost(11, "send eval")
  final val RECEIVE_EVAL_COST  = Cost(11, "receive eval")
  final val CHANNEL_EVAL_COST  = Cost(11, "channel eval")

  // The idea is that evaluation of `new x1, x2, …, xn in { }` should be charged depending
  // on the # of bindings and constant cost of evaluating `new … in  { … }` construct
  final val NEW_BINDING_COST  = Cost(2, "new binding")
  final val NEW_EVAL_COST     = Cost(10, "new eval")
  def newBindingsCost(n: Int) = Cost((NEW_BINDING_COST * n) + NEW_EVAL_COST, s"$n new bindings")

  final val MATCH_EVAL_COST = Cost(12, "match eval")

  def storageCostConsume(
      channels: Seq[Par],
      patterns: Seq[BindPattern],
      continuation: TaggedContinuation
  ): Cost = {
    val bodyCost = Some(continuation).collect {
      case TaggedContinuation(ParBody(ParWithRandom(body, _))) => storageCost(body)
    }
    storageCost(channels: _*) + storageCost(patterns: _*) + bodyCost.getOrElse(Cost(0))
  }

  def storageCostProduce(channel: Par, data: ListParWithRandom): Cost =
    storageCost(channel) + storageCost(data.pars: _*)

  private def storageCost[A <: StacksafeMessage[_]](as: A*): Cost =
    Cost(as.map(a => ProtoM.serializedSize(a).value).sum, "storage cost")

  def commEventStorageCost(channelsInvolved: Int): Cost = {
    val consumeCost  = eventStorageCost(channelsInvolved)
    val produceCosts = eventStorageCost(1) * channelsInvolved
    (consumeCost + produceCosts).copy(operation = "comm event storage cost")
  }

  def eventStorageCost(channelsInvolved: Int): Cost =
    Cost(eventHashStorageCost + channelsInvolved * channelHashStorageCost, "event storage cost")

  private val eventHashStorageCost   = Blake2b256Hash.length
  private val channelHashStorageCost = Blake2b256Hash.length

  // Even though we use Long for phlo limit we can't use Long.MaxValue here.
  // This is because in tests, when  we refund deployment we add phlos to the deployment's limit
  // which may result in overflow when we start with maximum value.
  // In normal scenario this will never happen because user would need to provide equivalent of Long.MaxValue
  // in REVs and this is more than there is REVs available.
  final val MAX_VALUE = Integer.MAX_VALUE
}
