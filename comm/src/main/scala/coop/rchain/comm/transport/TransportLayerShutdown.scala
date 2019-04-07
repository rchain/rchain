package coop.rchain.comm.transport

import coop.rchain.comm.protocol.routing.Protocol

class TransportLayerShutdown[F[_]](shutdown: Protocol => F[Unit]) {
  def apply(msg: Protocol): F[Unit] = shutdown(msg)
}
