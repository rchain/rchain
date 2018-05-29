package coop.rchain.shared

import java.nio.file.{Files, Path}

object PathOps {

  implicit class RichPath(value: Path) {

    def folderSize: Long =
      Files
        .walk(value)
        .mapToLong(p => {
          val f = p.toFile
          if (f.isFile)
            f.length
          else
            0
        })
        .sum()
  }
}
