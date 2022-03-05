package coop.rchain.sdk.block.data

import fs2.Stream

/**
  * High level module for acquiring blocks.
  *
  * @tparam B represents a block
  * @tparam BId represents block ID (hash)
  */
trait BlockRequester[F[_], B, BId] {
  def requestBlock(id: BId): F[Unit]

  def response: Stream[F, B]
}

object BlockRequester {
  def apply[F[_], B, BId](implicit instance: BlockRequester[F, B, BId]): instance.type = instance
}
