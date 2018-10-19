package coop.rchain.rspace

import java.nio.file.Path

import coop.rchain.shared.PathOps.RichPath

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
}
