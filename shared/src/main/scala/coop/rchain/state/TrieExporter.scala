package coop.rchain.state

import java.nio.ByteBuffer

// Defines basic operation to traverse tries and convert to path indexed list
trait TrieExporter[F[_]] {
  // Type of the key to uniquely defines the trie / in RSpace this is the hash of the trie
  type KeyHash
  // Type of the full path to the node
  // - it contains parent nodes with indexes and node itself at the end
  type NodePath = Seq[(KeyHash, Option[Byte])]

  // Get trie nodes with offset from start path and number of nodes
  // - skipping nodes can be expensive as taking nodes
  def getNodes(
      startPath: NodePath,
      skip: Int,
      take: Int
  ): F[Seq[TrieNode[KeyHash]]]

  // Get history values / from branch nodes in the trie
  def getHistoryItems[Value](
      keys: Seq[KeyHash],
      fromBuffer: ByteBuffer => Value
  ): F[Seq[(KeyHash, Value)]]

  // Get data values / from leaf nodes in the trie
  def getDataItems[Value](
      keys: Seq[KeyHash],
      fromBuffer: ByteBuffer => Value
  ): F[Seq[(KeyHash, Value)]]
}

final case class TrieNode[KeyHash](
    hash: KeyHash,
    isLeaf: Boolean,
    path: Seq[(KeyHash, Option[Byte])]
)
