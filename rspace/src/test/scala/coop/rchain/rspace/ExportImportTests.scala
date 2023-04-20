package coop.rchain.rspace

import cats.effect.IO
import cats.syntax.all._
import coop.rchain.metrics.{Metrics, NoopSpan, Span}
import coop.rchain.rspace.examples.StringExamples.implicits._
import coop.rchain.rspace.examples.StringExamples.{Pattern, Wildcard}
import coop.rchain.rspace.hashing.Blake2b256Hash
import coop.rchain.rspace.history.History.emptyRootHash
import coop.rchain.rspace.history.HistoryRepositoryInstances
import coop.rchain.rspace.state.exporters.RSpaceExporterItems
import coop.rchain.rspace.state.{RSpaceExporter, RSpaceImporter}
import coop.rchain.shared.ByteVectorOps.RichByteVector
import coop.rchain.shared.{Log, Serialize}
import coop.rchain.store.InMemoryStoreManager
import monix.execution.atomic.AtomicAny
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scodec.bits.ByteVector
import cats.effect.Ref

class ExportImportTests
    extends AnyFlatSpec
    with Matchers
    with InMemoryExportImportTestsBase[String, Pattern, String, String] {

  "export and import of one page" should "works correctly" in fixture {
    (space1, exporter1, importer1, space2, _, importer2) =>
      implicit val log: Log.NOPLog[IO] = new Log.NOPLog[IO]()
      val pageSize                     = 1000 // Match more than dataSize
      val dataSize: Int                = 10
      val startSkip: Int               = 0
      val range                        = 0 until dataSize
      val pattern                      = List(Wildcard)
      val continuation                 = "continuation"

      for {
        // Generate init data in space1
        _ <- range.toVector.traverse { i =>
              space1.produce(s"ch$i", s"data$i", persist = false)
            }
        initPoint <- space1.createCheckpoint()

        // Export 1 page from space1
        initStartPath = Vector((initPoint.root, none))
        exportData <- RSpaceExporterItems.getHistoryAndData(
                       exporter1,
                       initStartPath,
                       startSkip,
                       pageSize,
                       ByteVector(_)
                     )
        historyItems = exportData._1.items.toVector
        dataItems    = exportData._2.items.toVector

        // Validate exporting page
        _ <- RSpaceImporter.validateStateItems[IO](
              historyItems,
              dataItems,
              initStartPath,
              pageSize,
              startSkip,
              importer1.getHistoryItem
            )

        // Import page to space2
        _ <- importer2.setHistoryItems[ByteVector](historyItems, _.toDirectByteBuffer)
        _ <- importer2.setDataItems[ByteVector](dataItems, _.toDirectByteBuffer)
        _ <- importer2.setRoot(initPoint.root)
        _ <- space2.reset(initPoint.root)

        // Testing data in space2 (match all installed channels)
        _ <- range.toVector.traverse { i =>
              space2.consume(Seq(s"ch$i"), pattern, continuation, persist = false)
            }
        endPoint <- space2.createCheckpoint()

        _ = endPoint.root shouldBe emptyRootHash
      } yield ()
  }

  "multipage export" should "works correctly" in fixture {
    (space1, exporter1, importer1, space2, _, importer2) =>
      implicit val log: Log.NOPLog[IO] = new Log.NOPLog[IO]()
      val pageSize                     = 10
      val dataSize: Int                = 1000
      val startSkip: Int               = 0
      val range                        = 0 until dataSize
      val pattern                      = List(Wildcard)
      val continuation                 = "continuation"

      type Params = (
          Seq[(Blake2b256Hash, ByteVector)],  // HistoryItems
          Seq[(Blake2b256Hash, ByteVector)],  // DataItems
          Seq[(Blake2b256Hash, Option[Byte])] // StartPath
      )
      def multipageExport(params: Params): IO[Either[Params, Params]] =
        params match {
          case (historyItems, dataItems, startPath) =>
            for {
              // Export 1 page from space1
              exportData <- RSpaceExporterItems.getHistoryAndData(
                             exporter1,
                             startPath,
                             startSkip,
                             pageSize,
                             ByteVector(_)
                           )
              historyItemsPage = exportData._1.items
              dataItemsPage    = exportData._2.items
              lastPath         = exportData._1.lastPath

              // Validate exporting page
              _ <- RSpaceImporter.validateStateItems[IO](
                    historyItemsPage,
                    dataItemsPage,
                    startPath,
                    pageSize,
                    startSkip,
                    importer1.getHistoryItem
                  )

              r = (historyItems ++ historyItemsPage, dataItems ++ dataItemsPage, lastPath)
            } yield if (historyItemsPage.size < pageSize) r.asRight else r.asLeft
        }

      val initHistoryItems: Seq[(Blake2b256Hash, ByteVector)] = Seq.empty
      val initDataItems: Seq[(Blake2b256Hash, ByteVector)]    = Seq.empty
      val initChildNum: Option[Byte]                          = none

      for {
        // Generate init data in space1
        _ <- range.toVector.traverse { i =>
              space1.produce(s"ch$i", s"data$i", persist = false)
            }
        initPoint <- space1.createCheckpoint()

        // Multipage export from space1
        initStartPath  = Seq((initPoint.root, initChildNum))
        initExportData = (initHistoryItems, initDataItems, initStartPath)
        exportData     <- initExportData.tailRecM(multipageExport)
        historyItems   = exportData._1
        dataItems      = exportData._2

        // Import page to space2
        _ <- importer2.setHistoryItems[ByteVector](historyItems, _.toDirectByteBuffer)
        _ <- importer2.setDataItems[ByteVector](dataItems, _.toDirectByteBuffer)
        _ <- importer2.setRoot(initPoint.root)
        _ <- space2.reset(initPoint.root)

        // Testing data in space2 (match all installed channels)
        _ <- range.toVector.traverse { i =>
              space2.consume(Seq(s"ch$i"), pattern, continuation, persist = false)
            }
        endPoint <- space2.createCheckpoint()

        _ = endPoint.root shouldBe emptyRootHash
      } yield ()
  }

  // Attention! Skipped export is significantly slower than last path export.
  // But on the other hand, this allows you to work simultaneously with several nodes.
  "multipage export with skip" should "works correctly" in fixture {
    (space1, exporter1, importer1, space2, _, importer2) =>
      implicit val log: Log.NOPLog[IO] = new Log.NOPLog[IO]()
      val pageSize                     = 10
      val dataSize: Int                = 1000
      val startSkip: Int               = 0
      val range                        = 0 until dataSize
      val pattern                      = List(Wildcard)
      val continuation                 = "continuation"

      type Params = (
          Seq[(Blake2b256Hash, ByteVector)],   // HistoryItems
          Seq[(Blake2b256Hash, ByteVector)],   // DataItems
          Seq[(Blake2b256Hash, Option[Byte])], // StartPath
          Int                                  // Size of skip
      )
      def multipageExportWithSkip(params: Params): IO[Either[Params, Params]] =
        params match {
          case (historyItems, dataItems, startPath, skip) =>
            for {
              // Export 1 page from space1
              exportData <- RSpaceExporterItems.getHistoryAndData(
                             exporter1,
                             startPath,
                             skip,
                             pageSize,
                             ByteVector(_)
                           )
              historyItemsPage = exportData._1.items
              dataItemsPage    = exportData._2.items

              // Validate exporting page
              _ <- RSpaceImporter.validateStateItems[IO](
                    historyItemsPage,
                    dataItemsPage,
                    startPath,
                    pageSize,
                    skip,
                    importer1.getHistoryItem
                  )

              r = (
                historyItems ++ historyItemsPage,
                dataItems ++ dataItemsPage,
                startPath,
                skip + pageSize
              )
            } yield if (historyItemsPage.size < pageSize) r.asRight else r.asLeft
        }

      val initHistoryItems: Seq[(Blake2b256Hash, ByteVector)] = Seq.empty
      val initDataItems: Seq[(Blake2b256Hash, ByteVector)]    = Seq.empty
      val initChildNum: Option[Byte]                          = none

      for {
        // Generate init data in space1
        _ <- range.toVector.traverse { i =>
              space1.produce(s"ch$i", s"data$i", persist = false)
            }
        initPoint <- space1.createCheckpoint()

        // Multipage export with skip from space1
        initStartPath  = Seq((initPoint.root, initChildNum))
        initExportData = (initHistoryItems, initDataItems, initStartPath, startSkip)
        exportData     <- initExportData.tailRecM(multipageExportWithSkip)
        historyItems   = exportData._1
        dataItems      = exportData._2

        // Import page to space2
        _ <- importer2.setHistoryItems[ByteVector](historyItems, _.toDirectByteBuffer)
        _ <- importer2.setDataItems[ByteVector](dataItems, _.toDirectByteBuffer)
        _ <- importer2.setRoot(initPoint.root)
        _ <- space2.reset(initPoint.root)

        // Testing data in space2 (match all installed channels)
        _ <- range.toVector.traverse { i =>
              space2.consume(Seq(s"ch$i"), pattern, continuation, persist = false)
            }
        endPoint <- space2.createCheckpoint()

        _ = endPoint.root shouldBe emptyRootHash
      } yield ()
  }
}

trait InMemoryExportImportTestsBase[C, P, A, K] {
  import cats.effect.unsafe.implicits.global
  def fixture[S](
      f: (
          ISpace[IO, C, P, A, K],
          RSpaceExporter[IO],
          RSpaceImporter[IO],
          ISpace[IO, C, P, A, K],
          RSpaceExporter[IO],
          RSpaceImporter[IO]
      ) => IO[S]
  )(
      implicit
      sc: Serialize[C],
      sp: Serialize[P],
      sa: Serialize[A],
      sk: Serialize[K],
      m: Match[IO, P, A]
  ): S = {
    implicit val log: Log[IO]                  = Log.log[IO]
    implicit val metricsF: Metrics[IO]         = new Metrics.MetricsNOP[IO]()
    implicit val spanF: Span[IO]               = NoopSpan[IO]()
    implicit val kvm: InMemoryStoreManager[IO] = InMemoryStoreManager[IO]()

    (for {
      roots1   <- kvm.store("roots1")
      cold1    <- kvm.store("cold1")
      history1 <- kvm.store("history1")
      historyRepository1 <- HistoryRepositoryInstances.lmdbRepository[IO, C, P, A, K](
                             roots1,
                             cold1,
                             history1
                           )
      cache1        <- Ref.of[IO, HotStoreState[C, P, A, K]](HotStoreState[C, P, A, K]())
      historyReader <- historyRepository1.getHistoryReader(historyRepository1.root)
      store1 <- {
        val hr = historyReader.base
        HotStore[IO, C, P, A, K](cache1, hr).map(AtomicAny(_))
      }
      space1 = new RSpace[IO, C, P, A, K](
        historyRepository1,
        store1
      )
      exporter1 <- historyRepository1.exporter
      importer1 <- historyRepository1.importer

      roots2   <- kvm.store("roots2")
      cold2    <- kvm.store("cold2")
      history2 <- kvm.store("history2")
      historyRepository2 <- HistoryRepositoryInstances.lmdbRepository[IO, C, P, A, K](
                             roots2,
                             cold2,
                             history2
                           )
      cache2        <- Ref.of[IO, HotStoreState[C, P, A, K]](HotStoreState[C, P, A, K]())
      historyReader <- historyRepository2.getHistoryReader(historyRepository2.root)
      store2 <- {
        val hr = historyReader.base
        HotStore[IO, C, P, A, K](cache2, hr).map(AtomicAny(_))
      }
      space2 = new RSpace[IO, C, P, A, K](
        historyRepository2,
        store2
      )
      exporter2 <- historyRepository2.exporter
      importer2 <- historyRepository2.importer

      res <- f(space1, exporter1, importer1, space2, exporter2, importer2)
    } yield { res }).unsafeRunSync()
  }
}
