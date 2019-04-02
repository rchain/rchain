package coop.rchain.catscontrib

/**
  * Name ski comes from SKI combinator calculus (https://en.wikipedia.org/wiki/SKI_combinator_calculus)
  * It represents three functions, from which we are using two: constant function (kp) and identity function (id)
  *
  * Const and identity function are being used frequently in this code base (in any FP code base to be precise).
  * Thus we provide an alias for const and identity.
  * Inspired by ski package in slamdata/quasar codebase: κ and ι functions.
  */
package object ski {

  /**
    * Returns a function that takes number of arguments and always returns the last argument as a result
    */
  def kp[A, B](x: => B): A => B          = _ => x
  def kp2[A, B, C](x: => C): (A, B) => C = (_, _) => x

}
