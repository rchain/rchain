package coop.rchain.storage

package object util {

  /**
    * Runs a computation for its side-effects, discarding its value
    *
    * @param a A computation to run
    */
  def ignore[A](a: => A): Unit = {
    val _: A = a
    ()
  }

  /**
    * Executes a function `f` with a given [[AutoCloseable]] `a` as its argument,
    * returning the result of the function and closing the `a`
    *
    * Compare to Java's "try-with-resources"
    *
    * @param a A given resource implementing [[AutoCloseable]]
    * @param f A function that takes this resource as its argument
    */
  def withResource[A <: AutoCloseable, B](a: A)(f: A => B): B =
    try {
      f(a)
    } finally {
      a.close()
    }

  def withTransaction[A, B](txn: A)(f: A => B)(implicit t: Transaction[A]): B =
    try {
      val ret: B = f(txn)
      t.commit(txn)
      ret
    } catch {
      case ex: Throwable =>
        t.abort(txn)
        throw ex
    } finally {
      t.close(txn)
    }
}
