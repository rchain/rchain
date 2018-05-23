package coop.rchain.shared

object Resources {
  
  def withResource[A <: AutoCloseable, B](a: A)(f: A => B): B =
    try {
      f(a)
    } finally {
      a.close()
    }
}
