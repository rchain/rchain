package coop.rchain.rspace
import cats.Monoid
import coop.rchain.rspace.Match.MatchResult
import coop.rchain.rspace.Match.MatchResult.{Error, Found, NotFound}

/**
  * Type class for matching patterns with data.
  *
  * @tparam P A type representing patterns
  * @tparam E A type representing illegal state
  * @tparam S A type representing a state that is accumulated between matches
  * @tparam A A type representing data
  * @tparam R A type representing a match result
  */
trait Match[P, E, A, S, R] {

  def get(p: P, a: A): MatchResult[R, S, E]
}

object Match {
  sealed trait MatchResult[R, S, E] {
    def map[S2](f: S => S2): MatchResult[R, S2, E]
    def fold[C](fr: (S, R) => C, fs: S => C, fe: (S, E) => C): C = this match {
      case Found(s, v) => fr(s, v)
      case NotFound(s) => fs(s)
      case Error(s, e) => fe(s, e)
    }

    def toEither: Either[E, Option[R]] = this match {
      case Found(_, v) => Right(Some(v))
      case NotFound(_) => Right(None)
      case Error(_, e) => Left(e)
    }

    def isFound: Boolean = this match {
      case _: Found[R, S, E] => true
      case _                 => false
    }

    def isNotFound: Boolean = this match {
      case _: NotFound[R, S, E] => true
      case _                    => false
    }

    def isError: Boolean = this match {
      case _: Error[R, S, E] => true
      case _                 => false
    }
  }

  object MatchResult {
    final case class Found[R, S, E](state: S, value: R) extends MatchResult[R, S, E] {
      override def map[S2](f: S => S2): MatchResult[R, S2, E] = Found[R, S2, E](f(state), value)
    }
    final case class NotFound[R, S, E](state: S) extends MatchResult[R, S, E] {
      override def map[S2](f: S => S2): MatchResult[R, S2, E] = NotFound[R, S2, E](f(state))
    }
    final case class Error[R, S, E](state: S, err: E) extends MatchResult[R, S, E] {
      override def map[S2](f: S => S2): MatchResult[R, S2, E] = Error[R, S2, E](f(state), err)
    }

    def fromEither[E, R, S](either: Either[E, Option[R]], empty: S): MatchResult[R, S, E] =
      either match {
        case Left(e)        => Error(empty, e)
        case Right(None)    => NotFound(empty)
        case Right(Some(r)) => Found(empty, r)
      }
  }
}
