package coop.rchain.casper.util

import com.google.protobuf.ByteString
import cats._
import cats.implicits._
import coop.rchain.casper.protocol._
import coop.rchain.casper.util.rholang.RuntimeManager
import coop.rchain.models.{Expr, GPrivate, Par}
import coop.rchain.models.Expr.ExprInstance.GInt
import coop.rchain.models.rholang.implicits._
import coop.rchain.rholang.interpreter.{PrettyPrinter => RholangPrettyPrinter}
import coop.rchain.shared.Base16

object RSpaceUtil {

  def getDataAtPublicChannel[F[_]: FlatMap](hash: ByteString, channel: Long)(
      implicit runtimeManager: RuntimeManager[F]
  ): F[Seq[String]] = getDataAt[F](hash, GInt(channel))

  def getDataAtPublicChannel[F[_]: FlatMap](block: BlockMessage, channel: Long)(
      implicit runtimeManager: RuntimeManager[F]
  ): F[Seq[String]] = getDataAtPublicChannel[F](ProtoUtil.postStateHash(block), channel)

  def getDataAtPrivateChannel[F[_]: FlatMap](block: BlockMessage, channel: String)(
      implicit runtimeManager: RuntimeManager[F]
  ) = {
    val name = ByteString.copyFrom(
      Base16
        .unsafeDecode(channel)
    )
    getDataAt[F](ProtoUtil.postStateHash(block), GPrivate().withId(name))
  }

  def getDataAt[F[_]: FlatMap](hash: ByteString, channel: Par)(
      implicit runtimeManager: RuntimeManager[F]
  ) =
    for {
      data <- runtimeManager.getData(hash)(channel)
      res  = data.map(_.exprs.map(RholangPrettyPrinter().buildString)).flatten
    } yield (res)

}
