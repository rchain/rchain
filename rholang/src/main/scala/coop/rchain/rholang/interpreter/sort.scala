package coop.rchain.rholang.interpreter

case class ScoredTerm(term: Any, score: Seq[Int]) extends Ordered[ScoredTerm] {
  def compare(that: ScoredTerm) : Int = {
    def compareScore(s1: Seq[Int], s2: Seq[Int]) : Int = {
      (s1, s2) match {
        case (h1 :: t1, h2 :: t2) => {
          h1 - h2 match {
            case 0 => compareScore(t1,t2)
            case other => other
          }
        }
        case (List(_), _) => 1
        case (_, List(_)) => -1
        case (_, _) => 0
      }
    }
    compareScore(this.score, that.score)
  }
}
