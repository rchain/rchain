package coop.rchain.models.rholangn

import cats.Eval
import coop.rchain.models.rholangn.parmanager.Manager._

import java.util
import scala.math.Ordered.orderingToOrdered

/** Base trait for Rholang elements in the Reducer */
sealed trait RhoTypeN {

  /** Cryptographic hash code of this object */
  val rhoHash: Eval[Array[Byte]] = rhoHashFn(this).memoize

  /** The size of serialized bytes lazily evaluated with memoization */
  val serializedSize: Eval[Int] = serializedSizeFn(this).memoize

  /** Serialized bytes lazily evaluated with memoization */
  val serialized: Eval[Array[Byte]] = serializedFn(this, memoizeChildren = false).memoize

  /** True if the object or at least one of the nested objects non-concrete.
    * Such a object cannot be viewed as if it were a term.*/
  // TODO: Rename connectiveUsed for more clarity
  val connectiveUsed: Eval[Boolean] = connectiveUsedFn(this).memoize

  /** True if the object or at least one of the nested objects can be evaluated in Reducer */
  lazy val evalRequired: Boolean = evalRequiredFn(this)

  /** True if the object or at least one of the nested objects can be substituted in Reducer */
  lazy val substituteRequired: Boolean = substituteRequiredFn(this)

  override def equals(other: Any): Boolean = other match {
    case x: RhoTypeN => this.rhoHash.value sameElements x.rhoHash.value
    case _           => false
  }

  override def hashCode(): Int = this.rhoHash.value.hashCode()
}

/** Rholang element that can be processed in parallel, together with other elements */
sealed trait ParN extends RhoTypeN

object ParN {
  implicit val o: Ordering[Array[Byte]] = (a: Array[Byte], b: Array[Byte]) =>
    util.Arrays.compare(a, b)

  /**
    * Create a flatten parallel Par (ParProc) from par sequence.
    * See [[flattedPProc]] for more information.
    */
  def makeParProc(ps: Seq[ParN]): ParN = flattedPProc(ps)

  /** Combine two pars for their parallel execution */
  def combine(p1: ParN, p2: ParN): ParN = combinePars(p1, p2)

  def compare(p1: ParN, p2: ParN): Int = p1.rhoHash.value compare p2.rhoHash.value
  val ordering: Ordering[ParN]         = (p1: ParN, p2: ParN) => compare(p1, p2)
}

/** Basic rholang operations that can be executed in parallel*/
trait BasicN extends ParN

/** Expressions included in Rholang elements */
sealed trait ExprN extends ParN

/** Base types for Rholang expressions */
trait GroundN extends ExprN

/** Rholang collections */
trait CollectionN extends ExprN

/** Variables in Rholang (can be bound, free and wildcard) */
trait VarN extends ExprN

/** Operations in Rholang */
sealed trait OperationN extends ExprN

/** Operation with one par */
trait Operation1ParN extends OperationN {
  val p: ParN
}

/** Operation with two par */
trait Operation2ParN extends OperationN {
  val p1: ParN
  val p2: ParN
}

/** Other operations (e.g. method) */
trait OperationOtherN extends OperationN

/** Rholang unforgeable names (stored in internal environment map) */
trait UnforgeableN extends ParN {
  val v: Array[Byte]
}

/**
  * Connectives (bindings) are used in patterns to combine several conditions together or
  * to set a pattern with some specific Rholang type or variables.
  * */
trait ConnectiveN extends ParN

/** Connectives for simple types */
trait ConnectiveSTypeN extends ConnectiveN

/** Connectives for truth-functional operators */
trait ConnectiveFuncN extends ConnectiveN

/** Connectives for variables */
trait ConnectiveVarN extends ConnectiveN

/** Other types that can't be categorized */
trait OtherN extends ParN
