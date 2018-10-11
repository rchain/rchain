package coop.rchain.rholang.interpreter

import scala.collection.immutable.Map

case class Env[A] private (envMap: Map[Int, A], level: Int, shift: Int) {
  def put(a: A): Env[A] =
    Env(envMap + (level -> a), level + 1, shift)

  def get(k: Int): Option[A] =
    envMap.get((level + shift) - k - 1)

  def shift(j: Int): Env[A] =
    this.copy(shift = shift + j)
}

object Env {
  def apply[A]() = new Env[A](Map.empty[Int, A], 0, 0)

  def makeEnv[A](k: A*): Env[A] = k.foldLeft(Env[A]())((acc, newVal) => acc.put(newVal))
}
