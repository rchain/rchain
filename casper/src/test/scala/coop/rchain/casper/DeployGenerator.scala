package coop.rchain.casper

import coop.rchain.casper.protocol._
import coop.rchain.shared.{Log, LogSource, Time}
import com.google.protobuf.{ByteString, Int32Value, StringValue}
import cats._, cats.data._, cats.implicits._
import coop.rchain.rholang.interpreter.accounting

object DeployGenerator {
  def basicDeployData[F[_]: Monad: Time](id: Int): F[DeployData] =
    Time[F].currentMillis.map(
      now =>
        DeployData()
          .withUser(ByteString.EMPTY)
          .withTimestamp(now)
          .withTerm(s"@${id}!($id)")
          .withPhloLimit(accounting.MAX_VALUE)
    )
}
