package coop.rchain.rspace

import java.nio.file.Path

import scala.collection.immutable.Seq

import coop.rchain.shared.PathOps.RichPath
import org.scalacheck._
import org.scalacheck.Arbitrary._
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

}
