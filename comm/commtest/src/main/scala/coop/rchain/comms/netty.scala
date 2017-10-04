package coop.rchain.comm

// import io.netty.bootstrap.ServerBootstrap;

// import io.netty.channel.ChannelFuture;
// import io.netty.channel.ChannelInitializer;
// import io.netty.channel.ChannelOption;
// import io.netty.channel.EventLoopGroup;
// import io.netty.channel.nio.NioEventLoopGroup;
// import io.netty.channel.socket.SocketChannel;
// import io.netty.channel.socket.nio.NioServerSocketChannel;

// import scalaz.netty._
// import scalaz.stream._
// import scalaz.concurrent._
// import java.net.InetSocketAddress
// import scodec.bits.ByteVector

class NettyComm(p: Peer) extends Comm {
  // val bossGroup = new NioEventLoopGroup
  // val workerGroup = new NioEventLoopGroup

  // lazy val senders = peers map { p =>
  //   Netty connect p.toInetSocketAddress
  // }

  val senders: Array[Endpoint] = Array()

  override def send(data: Array[Byte]) =
    senders foreach { s =>
      ()
    }

  override def sendTo(data: Array[Byte], id: java.util.UUID) = ()

  override def recv(): Result =
    Error("Unimplemented")

  override def addPeer(p: Peer) = {}

  override def removePeer(p: Peer) = {}

  override def removePeer(id: java.util.UUID) = {}

  override def getPeers: Array[Peer] = Array()

  override def peer = p
}
