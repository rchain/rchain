package coop.rchain.rholang.interpreter

import scala.collection.immutable.Map

case class Env[A](envMap: Map[Int, A], level: Int, shift: Int) {
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
  def apply[A]() = new Env[A](Map.empty[Int, A], 0, 0)

  def makeEnv[A](k: A*): Env[A] = k.foldLeft(Env[A]())((acc, newVal) => acc.put(newVal))
}
