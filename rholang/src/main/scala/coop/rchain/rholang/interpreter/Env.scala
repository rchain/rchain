package coop.rchain.rholang.interpreter

import scala.collection.mutable

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

case class Env[A](val envMap: mutable.LinkedHashMap[Int, A], var level: Int, var shift: Int) {
  def this() = this(mutable.LinkedHashMap.empty[Int, A], 0, 0)
  def put(a: A): Unit = {
    envMap += (level -> a)
    level += 1
  }

  def put(a: A, b: A, k: A*): Unit = {
    put(a)
    put(b)
    for {
      newVal <- k
    } put(newVal)
  }

  // Still slightly uncertain about this.
  def merge(_env: Env[A]): Unit = {
    val renamed = _env.rename(level)
    envMap ++= renamed.envMap
    level = renamed.level
    shift += _env.shift
  }

  def rename(j: Int): Env[A] =
    Env(envMap = envMap map {
      case (k, data) => (k + j, data)
    }, level = level + j, shift = shift)

  def get(k: Int): Option[A] =
    envMap.get((level + shift) - k - 1)

  def shift(j: Int): Unit =
    shift += j
}

object Env {
  def apply[A]() = new Env[A]()

  def makeEnv[A](k: A*): Env[A] = {
    val result = Env[A]()
    for {
      newVal <- k
    } result.put(newVal)
    result
  }
}
