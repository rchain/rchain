package coop.rchain.rholang.interpreter

import coop.rchain.rholang.interpreter.debugger.DebugInfo

import scala.collection.immutable.Map

final case class Env[A] private (
    envMap: Map[Int, A],
    level: Int,
    shift: Int,
    debugInfo: DebugInfo
) {
  def put(a: A): Env[A] =
    Env(envMap + (level -> a), level + 1, shift, debugInfo)

  def get(k: Int): Option[A] =
    envMap.get((level + shift) - k - 1)

  def shift(j: Int): Env[A] =
    this.copy(shift = shift + j)
}

object Env {
  def apply[A]() =
    new Env[A](Map.empty[Int, A], level = 0, shift = 0, debugInfo = DebugInfo())

  def apply[A](debugInfo: DebugInfo) =
    new Env[A](Map.empty[Int, A], level = 0, shift = 0, debugInfo = debugInfo)

  def makeEnv[A](k: A*): Env[A] = k.foldLeft(Env[A]())((acc, newVal) => acc.put(newVal))
}
