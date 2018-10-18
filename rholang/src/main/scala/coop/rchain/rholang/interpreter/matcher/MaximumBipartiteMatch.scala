package coop.rchain.rholang.interpreter.matcher

import cats._
import cats.data.StateT
import cats.implicits._

import scala.Function.tupled
import scala.collection.immutable.Stream

object MaximumBipartiteMatch {
  def apply[P, T, R, F[_]: Monad](
      matchFun: (P, T) => F[Option[R]]
  ): MaximumBipartiteMatch[P, T, R, F] = {
    val fM = implicitly[Monad[F]]
    new MaximumBipartiteMatch[P, T, R, F] {
      private[matcher] override implicit val fMonad: Monad[F] = fM
      private[matcher] override val matchFunction             = matchFun
    }
  }
}

/**
  * Implements the MaximumBipartiteMatch algorithm, according to:
  * http://olympiad.cs.uct.ac.za/presentations/camp2_2017/bipartitematching-robin.pdf
  * (mainly the "Alternative approach" section), with one slight difference:
  * we demand that all elements of the passed Seq[P] are assigned a match.
  * Otherwise findMatch returns None via the effect F.
  * <p/>
  * The `matchFunction: (P, T) => F[Option[R]]` must return a `Some` for a matching
  * (P, T) pair, and a `None` otherwise. The values returned via the `Option[R]` return
  * type for each of the patterns and targets are going to be captured and returned
  * upon successful matching of the provided Seq[P] and Seq[T].
  * <p/>
  * All the effects produced by calling the provided `matchFunction`
  * are going to be retained in the final effect via which the algo returns.
  * <p/>
  * See type signatures for `machFunction` and `findMatches`.
  *
  * @tparam P type of pattern / the U set
  * @tparam T type of term / the V set
  * @tparam R type of custom results captured during the matching
  * @tparam F the target Monadic effect this algorithm is to be embedded into
  */
trait MaximumBipartiteMatch[P, T, R, F[_]] {

  private[matcher] implicit val fMonad: Monad[F]
  private[matcher] val matchFunction: (P, T) => F[Option[R]]

  private case class S(matches: Map[Candidate, (Pattern, R)], seenTargets: Set[Candidate])
  private type Pattern = (P, Seq[Candidate])

  //we're going to use maps and sets keyed with Candidates,
  //so have to make sure they are treated as distinct even if they're equal
  private type Candidate = Indexed[T]
  private case class Indexed[A](value: A, index: Int)

  def findMatches(patterns: Seq[P], targets: Seq[T]): F[Option[Seq[(T, P, R)]]] = {

    val ts: Seq[Candidate]      = targets.zipWithIndex.map(tupled(Indexed[T]))
    val ps: List[Pattern]       = patterns.toList.zip(Stream.continually(ts))
    val findMatches             = ps.forallM(MBM.resetSeen() >> findMatch(_))
    val result: F[(S, Boolean)] = findMatches.run(S(Map.empty, Set.empty))
    result.map {
      case (state, true) =>
        val matches: Seq[(Candidate, ((P, _), R))] = state.matches.toSeq
        Some(matches.map(tupled((t, p) => (t.value, p._1._1, p._2))))
      case _ => None
    }
  }

  private type MBM[A] = StateT[F, S, A]
  import MBM._

  private def findMatch(pattern: Pattern): MBM[Boolean] =
    pattern match {
      //there are no more candidates for this pattern, there's not a match
      case (_, Stream.Empty) => pure(false)
      case (p, candidate +: candidates) =>
        FlatMap[MBM].ifM(notSeen(candidate))(
          //that is a new candidate, let's try to match it
          liftF(matchFunction(p, candidate.value)).flatMap {
            case Some(matchResult) =>
              //this candidate matches the pattern, let's try to assign it a match
              addSeen(candidate) >> tryClaimMatch(candidate, pattern, matchResult)
            case None =>
              //this candidate doesn't match, proceed to the others
              findMatch((p, candidates))
          },
          //we've seen this candidate already, proceed to the others
          findMatch((p, candidates))
        )
    }

  private def tryClaimMatch(candidate: Candidate, pattern: Pattern, result: R): MBM[Boolean] =
    for {
      previousMatch <- getMatch(candidate)
      result <- previousMatch match {
                 case None =>
                   //we're first, we claim a match
                   claimMatch(candidate, pattern, result) *> pure(true)
                 case Some(previousPattern) =>
                   //try to find a different match for the previous pattern
                   FlatMap[MBM].ifM(findMatch(previousPattern))(
                     //if found, we can match current pattern with this candidate despite it being taken
                     claimMatch(candidate, pattern, result) *> pure(true),
                     //else, current pattern can't be matched with this candidate given the current matches, try others
                     findMatch(pattern)
                   )
               }
    } yield result

  private object MBM {

    def pure[A](a: A): MBM[A] = a.pure[MBM]

    def liftF[A](fa: F[A]): MBM[A] = StateT.liftF(fa)

    def resetSeen(): MBM[Unit] = StateT.modify[F, S](s => s.copy(seenTargets = Set.empty))

    def notSeen(candidate: Candidate): MBM[Boolean] =
      StateT.inspect[F, S, Boolean](!_.seenTargets.contains(candidate))

    def addSeen(candidate: Candidate): MBM[Unit] =
      StateT.modify[F, S](s => s.copy(seenTargets = s.seenTargets + candidate))

    def getMatch(candidate: Candidate): MBM[Option[Pattern]] =
      StateT.inspect(_.matches.get(candidate).map(_._1))

    def claimMatch(candidate: Candidate, pattern: Pattern, result: R): MBM[Unit] =
      StateT.modify[F, S](s => {
        val newMatch = (candidate, (pattern, result))
        s.copy(matches = s.matches + newMatch)
      })
  }
}
