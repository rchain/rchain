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

  def get(k: Int): Option[A] =
    envMap.get((level + shift) - k - 1)

  def shift(j: Int): Env[A] =
    this.copy(shift = shift + j)
}

object Env {
  def apply[A]() = new Env[A](Map.empty[Int, A], 0, 0)

  def makeEnv[A](k: A*): Env[A] = k.foldLeft(Env[A]())((acc, newVal) => acc.put(newVal))
}
