package coop.rchain.rspace.history

import org.lmdbjava.{EnvFlags}
import java.nio.file.Path

final case class StoreConfig(
    path: Path,
    mapSize: Long,
    maxDbs: Int = 2,
    maxReaders: Int = 2048,
    flags: List[EnvFlags] = List(EnvFlags.MDB_NOTLS, EnvFlags.MDB_NORDAHEAD)
)
