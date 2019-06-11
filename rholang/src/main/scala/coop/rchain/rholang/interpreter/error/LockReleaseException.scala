package coop.rchain.rholang.interpreter.error

final case class LockReleaseException(message: String) extends Throwable
