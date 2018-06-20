package coop.rchain.shared

object Language {

  /**
    * Runs a computation for its side-effects, discarding its value
    *
    * @param a A computation to run
    */
  def ignore[A](a: => A): Unit = {
    val _: A = a
    ()
  }
}
