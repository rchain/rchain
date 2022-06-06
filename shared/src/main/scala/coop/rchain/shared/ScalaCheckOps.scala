package coop.rchain.shared

import cats.syntax.all._
import monix.eval.Task
import org.scalacheck.effect.PropF
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.{Assertion, Assertions}

/**
  * Wrappers for writing property-based tests using the ScalaCheck Effect library.
  */
object ScalaCheckOps {

  def forAllF[T: Arbitrary, P](f: T => Task[P]): Task[Assertion] =
    PropF.forAllF(f.map(_.void)).check().map(r => Assertions.assert(r.passed, r.status.toString))

  def forAllF[T: Arbitrary, P](data: Gen[T])(f: T => Task[P]): Task[Assertion] =
    PropF
      .forAllF(data)(f.map(_.void))
      .check()
      .map(r => Assertions.assert(r.passed, r.status.toString))
}
