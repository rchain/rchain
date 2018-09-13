package coop.rchain.shared

object Resources {

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
}
