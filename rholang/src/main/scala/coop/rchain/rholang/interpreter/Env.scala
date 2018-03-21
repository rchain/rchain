package coop.rchain.rholang.interpreter

import scala.collection.immutable.Map

/* Env reifies the Env[A] = mutable.LinkedHashMap[Int,A] type alias.
   It extends HashMap with functions that manipulate
   the map based on level indices. */

/*
object Env {

  type Env[A] = mutable.LinkedHashMap[Int, A]

  def apply[A](a: A): Env[A] =
    DeBruijn(mutable.LinkedHashMap.empty[Int, A]) put a

  def apply[A](a: A, b: A, k: A*): Env[A] =
    DeBruijn(mutable.LinkedHashMap.empty[Int, A]) put (a, b, k: _*)

  def apply[A](elems: (Int, A)*): Env[A] =
    mutable.LinkedHashMap[Int, A](elems: _*)

  implicit class DeBruijn[A](env: Env[A]) {

    def level: Int = if (env.isEmpty) 0 else env.last._1 + 1

    def rename(j: Int): Env[A] = env map {
      case (k, data) => (k + j, data)
    }

    def put(a: A): Env[A] = env += (env.level -> a)

    def put(a: A, b: A, k: A*): Env[A] = (env.put(a).put(b) /: k) { (_env, data) =>
      _env put data
    }

    def merge(_env: Env[A]): Env[A] =
      env ++= _env.rename(env.level)

  }
}
 */

case class Env[A](val envMap: Map[Int, A], val level: Int, val shift: Int) {
  def this() = this(Map.empty[Int, A], 0, 0)
  def put(a: A): Env[A] =
    Env(envMap + (level -> a), level + 1, shift)

  def put(a: A, b: A, k: A*): Env[A] =
    k.foldLeft(put(a).put(b))(
      (acc, newVal) => acc.put(newVal)
    )

  def put(as: Seq[A]): Env[A] =
    as.foldLeft(this)(
      (acc, newVal) => acc.put(newVal)
    )

  // Still slightly uncertain about this.
  def merge(_env: Env[A]): Env[A] = {
    val renamed = _env.rename(level)
    Env(envMap ++ renamed.envMap, renamed.level, shift + _env.shift)
  }

  private def rename(j: Int): Env[A] =
    Env(envMap = envMap map {
      case (k, data) => (k + j, data)
    }, level = level + j, shift = shift)

  def get(k: Int): Option[A] =
    envMap.get((level + shift) - k - 1)

  def shift(j: Int): Env[A] =
    this.copy(shift = shift + j)
}

object Env {
  def apply[A]() = new Env[A]()

  def makeEnv[A](k: A*): Env[A] =
    k.foldLeft(Env[A]())(
      (acc, newVal) => acc.put(newVal)
    )
}
