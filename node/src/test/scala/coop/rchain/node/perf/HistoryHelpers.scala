package coop.rchain.node.perf

import cats.Parallel
import cats.effect.{Concurrent, ContextShift, Sync}
import cats.syntax.all._
import coop.rchain.casper.storage.RNodeKeyValueStoreManager
import coop.rchain.metrics.{Metrics, Span}
import coop.rchain.models.{BindPattern, ListParWithRandom, Par, TaggedContinuation}
import coop.rchain.node.perf.RSpaceTraversal.TreeNode
import coop.rchain.rholang.interpreter.storage.matchListPar
import coop.rchain.rholang.interpreter.{storage, RholangCLI}
import coop.rchain.rspace
import coop.rchain.rspace.RSpace
import coop.rchain.rspace.hashing.Blake2b256Hash
import coop.rchain.rspace.history.{History, HistoryInstances, HistoryStoreInstances}
import coop.rchain.rspace.state.RSpaceExporter
import coop.rchain.rspace.state.exporters.RSpaceExporterDisk
import coop.rchain.rspace.syntax._
import coop.rchain.shared.{Log, Stopwatch}
import coop.rchain.state.TrieNode
import coop.rchain.store.{KeyValueStore, LmdbDirStoreManager}

import java.nio.file.Path
import scala.concurrent.ExecutionContext

trait HistoryHelpers[F[_]] {

  type Deps =
    (ExecutionContext, Concurrent[F], ContextShift[F], Parallel[F], Log[F], Metrics[F], Span[F])

  def init: Deps

  implicit val (ec, cc, cs, par, lg, mt, sp) = init

  val legacySource: Boolean = false

  def log(msg: String) = Sync[F].delay(println(msg))

  val concurrency = Runtime.getRuntime.availableProcessors()

  // For state items validation
  implicit val codecPar                                           = storage.serializePar
  implicit val codecBind                                          = storage.serializeBindPattern
  implicit val codecPars                                          = storage.serializePars
  implicit val codecCont                                          = storage.serializeTaggedContinuation
  implicit val m: rspace.Match[F, BindPattern, ListParWithRandom] = matchListPar[F]

  def time[A](msg: String)(block: F[A]) =
    Stopwatch.time(log)(msg)(block)

  def hash(hex: String) = Blake2b256Hash.fromHex(hex)

  def createExporterWithStores(dir: Path): F[RSpaceExporter[F]] = {
    val kvMngr =
      if (legacySource)
        RNodeKeyValueStoreManager(dir, legacyRSpacePaths = legacySource)
      else RholangCLI.mkRSpaceStoreManager(dir)
    for {
      kvStore     <- kvMngr
      rspaceStore <- kvStore.rSpaceStores
      rspace <- RSpace.setUp[F, Par, BindPattern, ListParWithRandom, TaggedContinuation](
                 rspaceStore
               )
      (historyRepo, _) = rspace
      exporter         <- historyRepo.exporter
    } yield exporter
  }

  def createStore(dir: Path): F[KeyValueStore[F]] =
    for {
      kvStore     <- RholangCLI.mkRSpaceStoreManager(dir, mapSize = 10 * LmdbDirStoreManager.gb)
      rspaceStore <- kvStore.rSpaceStores
    } yield rspaceStore.history

  def transfer(importHash: Blake2b256Hash, srcDir: Path, dirTo: Path) =
    for {
      _        <- log(s"Export root hash $importHash to $dirTo")
      exporter <- createExporterWithStores(srcDir)

      _ <- RSpaceExporterDisk
            .writeToDisk[F, Par, BindPattern, ListParWithRandom, TaggedContinuation](
              exporter,
              importHash,
              dirTo,
              chunkSize = 20000
            )
    } yield ()

  def transferLabeled(
      name: String,
      importHash: Blake2b256Hash,
      srcDir: Path,
      dirToRoot: Path
  ) = {
    val dirTo = dirToRoot.resolve(s"rnode-$name/rspace")
    transfer(importHash, srcDir, dirTo)
  }

  type NodePath = Seq[(Blake2b256Hash, Option[Byte])]

  def getHistory(
      historyStore: KeyValueStore[F],
      root: Blake2b256Hash
  ): F[History[F]] = Sync[F].delay {
    val sourceTrieStore = HistoryStoreInstances.historyStore(historyStore)
    HistoryInstances.merging(root, sourceTrieStore)
  }

  def getNodes(
      historyStore: KeyValueStore[F],
      startPath: NodePath,
      skip: Int,
      take: Int
  ): F[Vector[TrieNode[Blake2b256Hash]]] = {
    val sourceTrieStore = HistoryStoreInstances.historyStore(historyStore)
    RSpaceExporter.traverseTrie(startPath, skip, take, sourceTrieStore.get)
  }

  def getLeafsSection(
      root: Blake2b256Hash,
      skip: Int,
      take: Int,
      historyStore: KeyValueStore[F]
  ): F[Vector[TreeNode[Blake2b256Hash]]] = {
    val sourceTrieStore = HistoryStoreInstances.historyStore(historyStore)
    RSpaceTraversal.getLeafs(root, skip, take, sourceTrieStore.get)
  }

  def getLeafs(
      root: Blake2b256Hash,
      historyStore: KeyValueStore[F]
  ): F[Vector[TreeNode[Blake2b256Hash]]] =
    getLeafsSection(root, skip = 0, take = Int.MaxValue, historyStore)
}
