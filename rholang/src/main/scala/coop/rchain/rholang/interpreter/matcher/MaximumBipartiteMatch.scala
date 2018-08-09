package coop.rchain.rholang.interpreter.matcher

import cats._
import cats.data.StateT
import cats.implicits._

object MaximumBipartiteMatch {
  def apply[P, T, F[_]: Monad](matchFun: (P, T) => F[Boolean]): MaximumBipartiteMatch[P, T, F] = {
    val fM = implicitly[Monad[F]]
    new MaximumBipartiteMatch[P, T, F] {
      private[matcher] override implicit val fMonad: Monad[F] = fM
      private[matcher] override val matchFunction             = matchFun
    }
  }
}

  /**
  * Implements the MaximumBipartiteMatch algorithm, according to:
  * http://olympiad.cs.uct.ac.za/presentations/camp2_2017/bipartitematching-robin.pdf
  * (mainly the "Alternative approach" section)
  * <p/>
  * All the effects produced by calling the provided `matchFunction`
  * are going to be retained in the final effect via which the algo returns.
  * See type signatures for `machFunction` and `findMatches`.
  *
  * @tparam P type of pattern / the U set
  * @tparam T type of term / the V set
  * @tparam F the target Monadic effect this algorithm is to be embedded into
  */
trait MaximumBipartiteMatch[P, T, F[_]] {

  private[matcher] implicit val fMonad: Monad[F]
  private[matcher] val matchFunction: (P, T) => F[Boolean]

  private case class S(matches: Map[T, Pattern], seenTargets: Set[T])
  type Pattern    = (P, Candidates)
  type Candidates = Seq[T]

  def findMatches(
      patterns: List[Pattern],
  ): F[Option[Seq[T]]] = {
    val findMatches             = patterns.forallM(MBM.resetSeen() >> findMatch(_))
    val result: F[(S, Boolean)] = findMatches.run(S(Map.empty, Set.empty))
    result.map {
      case (matches: S, matchSuccessful: Boolean) =>
        Some(matches.matches.keys.toSeq).filter(_ => matchSuccessful)
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
          FlatMap[MBM].ifM(liftF(matchFunction(p, candidate)))(
            //this candidate matches the pattern, let's try to assign it a match
            addSeen(candidate) >> tryClaimMatch(p, candidate, pattern),
            //this candidate doesn't match, proceed to the others
            findMatch((p, candidates))
          ),
          //we've seen this candidate already, proceed to the others
          findMatch((p, candidates)),
        )
    }

  private def tryClaimMatch(p: P, candidate: T, pattern: Pattern): MBM[Boolean] =
    for {
      previousMatch <- getMatch(candidate)
      result <- previousMatch match {
                 case None =>
                   //we're first, we claim a match
                   claimMatch(candidate, pattern)
                 case Some(previousPattern) =>
                   //try to find a different match for the previous pattern
                   FlatMap[MBM].ifM(findMatch(previousPattern))(
                     //if found, we can match current pattern with this candidate despite it being taken
                     claimMatch(candidate, pattern),
                     //else, current pattern can't be matched with this candidate given the current matches, try others
                     findMatch(pattern)
                   )
               }
    } yield result

  private object MBM {

    def pure[A](a: A): MBM[A] = a.pure[MBM]

    def liftF[A](fa: F[A]): MBM[A] = StateT.liftF(fa)

    def resetSeen(): MBM[Unit] = StateT.modify[F, S](s => s.copy(seenTargets = Set.empty))

    def notSeen(t: T): MBM[Boolean] = StateT.inspect[F, S, Boolean](!_.seenTargets.contains(t))

    def addSeen(t: T): MBM[Unit] = StateT.modify[F, S](s => s.copy(seenTargets = s.seenTargets + t))

    def getMatch(t: T): MBM[Option[Pattern]] = StateT.inspect(_.matches.get(t))

    def claimMatch(candidate: T, pattern: Pattern): MBM[Boolean] =
      for {
        _ <- StateT.modify[F, S](s => s.copy(matches = s.matches + (candidate -> pattern)))
      } yield true
  }
}
