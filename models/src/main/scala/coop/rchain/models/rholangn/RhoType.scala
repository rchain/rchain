package coop.rchain.models.rholangn

import cats.Eval
import coop.rchain.models.rholangn.parmanager.Manager._

/** Base trait for Rholang elements in the Reducer */
sealed trait RhoTypeN {

  /** Cryptographic hash code of this object */
  lazy val rhoHash: Eval[Array[Byte]] = rhoHashFn(this).memoize

  /** The size of serialized bytes lazily evaluated with memoization */
  val serializedSize: Eval[Int] = serializedSizeFn(this).memoize

  /** Serialized bytes lazily evaluated with memoization */
  val serialized: Eval[Array[Byte]] = serializedFn(this, memoizeChildren = false).memoize

  /** True if the object or at least one of the nested objects non-concrete.
    * Such a object cannot be viewed as if it were a term.*/
  // TODO: Rename connectiveUsed for more clarity
  lazy val connectiveUsed: Eval[Boolean] = connectiveUsedFn(this)

  /** True if the object or at least one of the nested objects can be evaluated in Reducer */
  lazy val evalRequired: Boolean = evalRequiredFn(this)

  /** True if the object or at least one of the nested objects can be substituted in Reducer */
  lazy val substituteRequired: Boolean = substituteRequiredFn(this)

  override def equals(x: Any): Boolean = parmanager.Manager.equals(this, x)
}

/* TODO: In the future, it is necessary to append the classification.
         Add main types and ground types.
         Ground types must be part of expressions, and expressions are part of the main types.
 */
/** Auxiliary elements included in other pairs */
trait AuxParN extends RhoTypeN

/** Rholang element that can be processed in parallel, together with other elements */
sealed trait ParN extends RhoTypeN

object ParN {

  /**
    * Create a flatten parallel Par (ParProc) from par sequence.
    * See [[flattedPProc]] for more information.
    */
  def makeParProc(ps: Seq[ParN]): ParN = flattedPProc(ps)

  /** Combine two pars for their parallel execution */
  def combine(p1: ParN, p2: ParN): ParN = combinePars(p1, p2)

  def compare(p1: ParN, p2: ParN): Int = comparePars(p1, p2)
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
