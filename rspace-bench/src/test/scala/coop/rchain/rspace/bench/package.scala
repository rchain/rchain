package coop.rchain.rspace

import cats.effect.IO

import java.io.{FileNotFoundException, InputStreamReader}
import java.nio.file.Path
import com.google.common.io.CharStreams
import coop.rchain.crypto.hash.Blake2b512Random
import coop.rchain.models.Par
import coop.rchain.rholang.interpreter.{Reduce, RhoRuntime}

import scala.collection.immutable.Seq
import coop.rchain.shared.PathOps.RichPath
import org.scalacheck._
import org.scalacheck.rng.Seed
import org.scalacheck.Gen.Parameters

package object bench {

  val BenchStorageDirPrefix = "rspace-bench-"

  /**
    * until we add flag 'delete_lmdb_dir_on_close' for benchmarks and unit-tests
    * this prevents periodic out of disk space failures
    */
  def deleteOldStorage(dbDir: Path): Unit =
    dbDir.getParent.toFile.listFiles
      .filter(dir => dir.isDirectory && (dir.toPath != dbDir))
      .filter(_.getName.startsWith(BenchStorageDirPrefix))
      .foreach(
        dir =>
          try {
            println(s"deleting... $dir")
            dir.toPath.recursivelyDelete()
          } catch {
            case _: Exception =>
          }
      )

  def generateSeq[A: Arbitrary](
      innerElementsSize: Int,
      initSeed: Long = 123456789L,
      seqSize: Int = 100
  ): Seq[A] = {
    val params = Parameters.default.withSize(innerElementsSize)
    (1 to seqSize).map(
      i => implicitly[Arbitrary[A]].arbitrary.apply(params, Seed(initSeed + i)).get
    )
  }

  def resourceFileReader(path: String): String = {
    var reader: InputStreamReader = null
    try {
      reader = new InputStreamReader(
        Option(getClass.getResourceAsStream(path))
          .getOrElse(throw new FileNotFoundException(path))
      )
      CharStreams.toString(reader)
    } finally {
      reader.close()
    }
  }

  def processErrors(errors: Vector[Throwable]): Vector[Throwable] = {
    if (errors.nonEmpty) {
      errors.foreach(_.printStackTrace())
      throw new RuntimeException(
        errors
          .map(_.toString())
          .mkString("Errors received during evaluation:\n", "\n", "\n")
      )
    }
    errors
  }

  def createTest(t: Option[Par])(
      implicit runtime: RhoRuntime[IO],
      rand: Blake2b512Random
  ): IO[Unit] =
    t match {
      case Some(par) => runtime.inj(par)
      case None      => IO.delay(())
    }
}
