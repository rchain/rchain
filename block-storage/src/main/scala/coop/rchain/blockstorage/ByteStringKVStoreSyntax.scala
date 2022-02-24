package coop.rchain.blockstorage

import cats.effect.Sync
import cats.syntax.all._
import coop.rchain.casper.PrettyPrinter
import coop.rchain.models.Validator.Validator
import coop.rchain.store.KeyValueTypedStore
import coop.rchain.shared.syntax._

trait ByteStringKVStoreSyntax {
  implicit final def blockStorageSyntaxByteStringKVTypedStore[F[_], V](
      store: KeyValueTypedStore[F, Validator, V]
  ): ByteStringKVStoreOps[F, V] = new ByteStringKVStoreOps[F, V](store)
}

final case class ByteStringKVInconsistencyError(message: String) extends Exception(message)

final class ByteStringKVStoreOps[F[_], V](
    // KeyValueTypedStore extensions / syntax
    private val store: KeyValueTypedStore[F, Validator, V]
) extends AnyVal {
  def getUnsafe(key: Validator)(
      implicit f: Sync[F],
      line: sourcecode.Line,
      file: sourcecode.File,
      enclosing: sourcecode.Enclosing
  ): F[V] = {
    def source = s"${file.value}:${line.value} ${enclosing.value}"
    def errMsg = s"ByteStringKVStore is missing key ${PrettyPrinter.buildString(key)}\n $source"
    store.get1(key) >>= (_.liftTo(ByteStringKVInconsistencyError(errMsg)))
  }
}
