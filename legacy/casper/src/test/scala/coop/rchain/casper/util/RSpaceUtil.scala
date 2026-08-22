package coop.rchain.casper.util

import cats.FlatMap
import cats.syntax.all._
import com.google.protobuf.ByteString
import coop.rchain.casper.protocol._
import coop.rchain.casper.rholang.RuntimeManager
import coop.rchain.models.Expr.ExprInstance.GInt
import coop.rchain.models.rholang.implicits._
import coop.rchain.models.syntax._
import coop.rchain.models.{GPrivate, Par}
import coop.rchain.rholang.interpreter.{PrettyPrinter => RholangPrettyPrinter}

object RSpaceUtil {

  def getDataAtPublicChannel[F[_]: FlatMap](hash: ByteString, channel: Long)(
      implicit runtimeManager: RuntimeManager[F]
  ): F[Seq[String]] = getDataAt[F](hash, GInt(channel))

  def getDataAtPublicChannel[F[_]: FlatMap](block: BlockMessage, channel: Long)(
      implicit runtimeManager: RuntimeManager[F]
  ): F[Seq[String]] = getDataAtPublicChannel[F](block.postStateHash, channel)

  def getDataAtPrivateChannel[F[_]: FlatMap](block: BlockMessage, channel: String)(
      implicit runtimeManager: RuntimeManager[F]
  ) = {
    val name = channel.unsafeHexToByteString
    getDataAt[F](block.postStateHash, GPrivate().withId(name))
  }

  def getDataAt[F[_]: FlatMap](hash: ByteString, channel: Par)(
      implicit runtimeManager: RuntimeManager[F]
  ) =
    for {
      data <- runtimeManager.getData(hash)(channel)
      res  = data.map(_.exprs.map(RholangPrettyPrinter().buildString)).flatten
    } yield (res)

}
