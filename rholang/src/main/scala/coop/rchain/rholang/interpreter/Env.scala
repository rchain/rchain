package coop.rchain.rholang.interpreter

import scala.collection.mutable

/* Env reifies the Env[A] = mutable.LinkedHashMap[Int,A] type alias.
   It extends HashMap with functions that manipulate
   the map based on level indices. */

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
