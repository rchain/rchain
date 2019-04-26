package coop.rchain.casper.util.rholang

final case class InsufficientRevError(required: Long, actual: Long) extends Throwable
