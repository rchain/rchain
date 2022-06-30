package coop.rchain.rholang.interpreter

import cats.effect.Sync
import cats.syntax.all._
import coop.rchain.rholang.interpreter.RhoRuntime.RhoHistoryRepository
import coop.rchain.rholang.interpreter.merging.RholangMergingLogic.convertToReadNumber
import coop.rchain.rspace.hashing.Blake2b256Hash
import coop.rchain.rspace.syntax._

trait RhoHistoryRepositorySyntax {
  implicit final def rhoHistoryRepositoryRholangSyntax[F[_]](
      hr: RhoHistoryRepository[F]
  ): RhoHistoryRepositoryOps[F] = new RhoHistoryRepositoryOps(hr)
}

final class RhoHistoryRepositoryOps[F[_]](private val hr: RhoHistoryRepository[F]) extends AnyVal {
  def readMergeableValues(
      baseState: Blake2b256Hash,
      channelHashes: Set[Blake2b256Hash]
  )(implicit sync: Sync[F]): F[Map[Blake2b256Hash, Long]] =
    for {
      historyReader <- hr.getHistoryReader(baseState)
      baseReader    = historyReader.readerBinary
      baseGetData   = baseReader.getData(_: Blake2b256Hash).map(_.map(_.decoded))
      read1 = convertToReadNumber[F](baseGetData)
        .apply(_: Blake2b256Hash)
        .map(res => res.getOrElse(0L))
      r <- channelHashes.toList.traverse(ch => read1(ch).map(ch -> _)).map(_.toMap)
    } yield r
}
