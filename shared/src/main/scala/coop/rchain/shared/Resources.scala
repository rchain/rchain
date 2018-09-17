package coop.rchain.shared
import scala.util.control.NonFatal

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
  def withResource[A <: AutoCloseable, B](a: => A)(f: A => B): B = {
    val resource: A = a
    require(resource != null, "resource is null")
    var exception: Throwable = null
    try {
      f(resource)
    } catch {
      case NonFatal(e) =>
        exception = e
        throw e
    } finally {
      closeAndAddSuppressed(exception, resource)
    }
  }

  private def closeAndAddSuppressed(e: Throwable, resource: AutoCloseable): Unit =
    if (e != null) {
      try {
        resource.close()
      } catch {
        case NonFatal(suppressed) =>
          e.addSuppressed(suppressed)
      }
    } else {
      resource.close()
    }
}
