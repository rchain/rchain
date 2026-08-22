package coop.rchain.sdk.block

import fs2.Stream

/**
  * High level module for acquiring blocks.
  *
  * TODO: consider exposing intermediate block status (in progress, failed, ...)
  *
  * @tparam B represents a block
  * @tparam BId represents a block ID (hash)
  */
trait BlockRequester[F[_], B, BId] {
  def requestBlock(id: BId): F[Unit]

  def response: Stream[F, B]
}

object BlockRequester {
  def apply[F[_], B, BId](implicit instance: BlockRequester[F, B, BId]): instance.type = instance
}
