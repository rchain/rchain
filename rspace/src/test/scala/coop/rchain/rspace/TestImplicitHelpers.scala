package coop.rchain.rspace
import cats.Id
import coop.rchain.rspace.Match.MatchResult
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

  implicit def matchResultDefinitionScalatest[R, S, E]: Definition[Id[MatchResult[R, S, E]]] =
    new Definition[Id[MatchResult[R, S, E]]] {
      override def isDefined(thing: Id[MatchResult[R, S, E]]): Boolean =
        thing.isFound
    }

  implicit def matchResultToEither[R, S, E](m: MatchResult[R, S, E]): Either[E, Option[R]] =
    m.toEither

  implicit def matchResultIdToEitherId[R, S, E](
      m: Id[MatchResult[R, S, E]]): Id[Either[E, Option[R]]] =
    m.toEither
}
