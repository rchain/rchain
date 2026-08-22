package coop.rchain.rspace
import cats.Id
import org.scalatest.enablers.Definition

//noinspection ConvertExpressionToSAM
trait TestImplicitHelpers {
  // Some helpers for usage only in the tests -- save us A LOT of explicit casting from Either to Option
  // it is safe because left type of `Either` is `Nothing` -- we don't expect any invalid states from the matcher
  implicit def eitherDefinitionScalatest[E, A]: Definition[Id[Either[E, Option[A]]]] =
    new Definition[Id[Either[E, Option[A]]]] {
      override def isDefined(thing: Id[Either[E, Option[A]]]): Boolean =
        thing.right.get.isDefined
    }
}
