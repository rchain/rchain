package coop.rchain.rholang.interpreter

import scala.collection.mutable

/* Env reifies the Env[A] = mutable.LinkedHashMap[Int,A] type alias.
   It extends HashMap with functions that manipulate
   the map based on level indices. */

object Env {

  type Env[A] = mutable.LinkedHashMap[Int, A]

  def apply[A](a: A): Env[A] =
    DeBruijn(mutable.LinkedHashMap.empty[Int, A]) + a

  def apply[A](a: A, b: A, k: A*): Env[A] =
    DeBruijn(mutable.LinkedHashMap.empty[Int, A]) + (a, b, k: _*)


  def apply[A](elems: (Int, A)*): Env[A] =
    mutable.LinkedHashMap[Int, A](elems: _*)

  implicit class DeBruijn[A](env: mutable.LinkedHashMap[Int, A]) {

    def level: Int = if (env.isEmpty) 0 else env.last._1 + 1

    def rename(j: Int): mutable.LinkedHashMap[Int, A] = env map {
      case (k, data) => (k + j, data)
    }

    def +(a: A): mutable.LinkedHashMap[Int, A] = env += (env.level -> a)

    def +(a: A, b: A, k: A*): mutable.LinkedHashMap[Int, A] = ((env + a + b) /: k) { (_env, data) =>
      _env + data
    }

    def merge(_env: mutable.LinkedHashMap[Int, A]): mutable.LinkedHashMap[Int, A] =
      env ++= _env.rename(env.level)

  }
}
