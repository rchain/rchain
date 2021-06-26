package coop.rchain.rspace

import coop.rchain.rspace.hashing.Blake2b256Hash
import coop.rchain.rspace.internal.{Datum, WaitingContinuation}
import scodec.bits.ByteVector

sealed trait HotStoreTrieAction

sealed trait TrieInsertAction extends HotStoreTrieAction

final case class TrieInsertProduce[A](hash: Blake2b256Hash, data: Seq[Datum[A]])
    extends TrieInsertAction

final case class TrieInsertJoins[C](hash: Blake2b256Hash, joins: Seq[Seq[C]])
    extends TrieInsertAction

final case class TrieInsertConsume[P, K](
    hash: Blake2b256Hash,
    continuations: Seq[WaitingContinuation[P, K]]
) extends TrieInsertAction

/** actions flavours with data encoded into ByteVector*/
final case class TrieInsertBinaryProduce(hash: Blake2b256Hash, data: Seq[ByteVector])
    extends TrieInsertAction

final case class TrieInsertBinaryJoins(hash: Blake2b256Hash, joins: Seq[ByteVector])
    extends TrieInsertAction

final case class TrieInsertBinaryConsume(hash: Blake2b256Hash, continuations: Seq[ByteVector])
    extends TrieInsertAction

sealed trait TrieDeleteAction extends HotStoreTrieAction

final case class TrieDeleteProduce(hash: Blake2b256Hash) extends TrieDeleteAction

final case class TrieDeleteJoins(hash: Blake2b256Hash) extends TrieDeleteAction

final case class TrieDeleteConsume(hash: Blake2b256Hash) extends TrieDeleteAction
