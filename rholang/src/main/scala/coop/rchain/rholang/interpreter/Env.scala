package coop.rchain.rholang.interpreter

import scala.collection.immutable.SortedMap

/* Env reifies the Env[A] = SortedMap[Int,A] type alias.
   It extends SortedMap with functions that manipulate
   the map based on level indices. */

object Env {

  type Env[A] = SortedMap[Int, A]

  def apply[A](a: A): Env[A] =
    DeBruijn(SortedMap.empty[Int, A]) put a

  def apply[A](a: A, b: A, k: A*): Env[A] =
    DeBruijn(SortedMap.empty[Int, A]) put (a +: b +: k: _*)

  def apply[A](elems: (Int, A)*): Env[A] =
    SortedMap[Int, A](elems: _*)

  implicit class DeBruijn[A](env: Env[A]) {

    def level: Int = if (env.isEmpty) 0 else env.last._1 + 1

    def rename(j: Int): Env[A] = env map {
      case (k, data) => (k + j, data)
    }

    def put(a: A): Env[A] = env + (env.level -> a)

    def put(k: A*): Env[A] = (env /: k) { (_env, data) =>
      _env put data
    }

    def merge(_env: Env[A]): Env[A] =
      env ++ _env.rename(env.level)

  }
}
