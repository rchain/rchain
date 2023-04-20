package coop.rchain.shared

object Language {

  /**
    * Runs a computation for its side-effects, discarding its value
    *
    * @param a A computation to run
    */
  def ignore[A](a: => A): Unit = {
    val _ = a
    ()
  }

  def removeIndex[E](col: Seq[E], index: Int): Seq[E] = {
    val (l1, l2) = col splitAt index
    (l1 ++ l2.drop(1))
  }

}
