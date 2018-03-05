package coop.rchain.catscontrib

import scala.concurrent.duration.{Duration, MILLISECONDS}
import coop.rchain.comm._, CommError._
import cats._, cats.data._, cats.implicits._
import Catscontrib._

// TODO work in progress
trait Network[F[_]] {
  def roundTrip(msg: ProtocolMessage,
                remote: ProtocolNode,
                timeout: Duration = Duration(500, MILLISECONDS)): F[CommErr[ProtocolMessage]]

  def commSend(data: Seq[Byte], peer: PeerNode): F[CommErr[Unit]]

  def addNode(node: PeerNode): F[Unit]

  def broadcast(msg: ProtocolMessage): F[Seq[CommErr[Unit]]]
}
