package coop.rchain.state

import java.nio.ByteBuffer

trait TrieImporter[F[_]] {
  // Type of the key to uniquely defines the trie / in RSpace this is the hash of the trie
  type KeyHash

  // Set history values / branch nodes in the trie
  def setHistoryItems[Value](
      data: Seq[(KeyHash, Value)],
      toBuffer: Value => ByteBuffer
  ): F[Unit]

  // Set data values / leaf nodes in the trie
  def setDataItems[Value](
      data: Seq[(KeyHash, Value)],
      toBuffer: Value => ByteBuffer
  ): F[Unit]

  // Set current root hash
  def setRoot(key: KeyHash): F[Unit]
}
