package coop.rchain.rspace
import scala.collection.immutable.Seq

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
    * Executes a function `f` with a given `a` as its argument,
    * returning the result of the function and closing the `a`
    *
    * Compare to Java's "try-with-resources"
    *
    * @param a A given resource that will be closed if it implements [[AutoCloseable]]
    * @param f A function that takes this resource as its argument
    */
  def optionalWithResource[A, B](a: A)(f: A => B): B =
    try {
      f(a)
    } finally {
      a match {
        case ac: AutoCloseable => ac.close()
        case _                 =>
      }
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

  /** Drops the 'i'th element of a Seq.
    */
  def dropIndex[T](xs: Seq[T], n: Int): Seq[T] = {
    val (l1, l2) = xs splitAt n
    l1 ++ (l2 drop 1)
  }

  /**
    * scala cats sequence
    * returns None if any element in the list is None, and return Some of values in the list otherwise
    */
  def sequence[T](xs: Seq[Option[T]]): Option[Seq[T]] =
    //return is bad operator, however fold and recursive implementations
    //generate enormous memory traffic due to Seq concatenations
    Some(xs.map {
      case None    => return None
      case Some(x) => x
    })
}
