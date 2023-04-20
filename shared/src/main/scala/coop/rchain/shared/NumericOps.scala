package coop.rchain.shared

object NumericOps {
  def by[B, A](f: B => A, g: A => B)(implicit ev: Numeric[A]): Numeric[B] =
    new Numeric[B] {
      override def plus(x: B, y: B): B =
        g(ev.plus(f(x), f(y)))

      override def minus(x: B, y: B): B =
        g(ev.minus(f(x), f(y)))

      override def times(x: B, y: B): B =
        g(ev.times(f(x), f(y)))

      override def negate(x: B): B =
        g(ev.negate(f(x)))

      override def fromInt(x: Int): B =
        g(ev.fromInt(x))

      override def toInt(x: B): Int =
        ev.toInt(f(x))

      override def toLong(x: B): Long =
        ev.toLong(f(x))

      override def toFloat(x: B): Float =
        ev.toFloat(f(x))

      override def toDouble(x: B): Double =
        ev.toDouble(f(x))

      override def compare(x: B, y: B): Int =
        ev.compare(f(x), f(y))

      override def parseString(str: String): Option[B] =
        ev.parseString(str).map(g)
    }

}
