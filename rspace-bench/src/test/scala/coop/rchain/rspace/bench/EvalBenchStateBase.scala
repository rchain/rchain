package coop.rchain.rspace.bench

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import coop.rchain.crypto.hash.Blake2b512Random
import coop.rchain.metrics
import coop.rchain.metrics.{Metrics, NoopSpan, Span}
import coop.rchain.models.Par
import coop.rchain.rholang.Resources
import coop.rchain.rholang.interpreter.{ParBuilderUtil, RhoRuntime, RholangCLI}
import coop.rchain.rspace.syntax.rspaceSyntaxKeyValueStoreManager
import coop.rchain.rholang.interpreter.compiler.Compiler
import coop.rchain.shared.Log
import org.openjdk.jmh.annotations.{Setup, TearDown}

import java.io.{FileNotFoundException, InputStreamReader}
import java.nio.file.{Files, Path}

trait EvalBenchStateBase {
  private lazy val dbDir: Path          = Files.createTempDirectory("rchain-storage-test-")
  implicit val logF: Log[IO]            = new Log.NOPLog[IO]
  implicit val noopMetrics: Metrics[IO] = new metrics.Metrics.MetricsNOP[IO]
  implicit val noopSpan: Span[IO]       = NoopSpan[IO]()
  implicit val kvm                      = RholangCLI.mkRSpaceStoreManager[IO](dbDir).unsafeRunSync()

  val rhoScriptSource: String

  val store                       = kvm.rSpaceStores.unsafeRunSync()
  lazy val spaces                 = Resources.createRuntimes[IO](store).unsafeRunSync()
  val (runtime, replayRuntime, _) = spaces

  val rand: Blake2b512Random = Blake2b512Random.defaultRandom
  var term: Option[Par]      = None

  @Setup
  def doSetup(): Unit = {
    deleteOldStorage(dbDir)

    term =
      Compiler[IO].sourceToADT(resourceFileReader(rhoScriptSource)).attempt.unsafeRunSync() match {
        case Right(par) => Some(par)
        case Left(err)  => throw err
      }
  }

  @TearDown
  def tearDown(): Unit = ()

  def resourceFileReader(path: String): InputStreamReader =
    new InputStreamReader(
      Option(getClass.getResourceAsStream(path))
        .getOrElse(throw new FileNotFoundException(path))
    )
}
