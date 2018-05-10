package coop.rchain.rspace.pure
import java.nio.ByteBuffer
import java.nio.file.{Path}

import org.lmdbjava.DbiFlags.MDB_CREATE
import org.lmdbjava.{Dbi, Env}

class LMDBContext(val env: Env[ByteBuffer],
                  val dbKeys: Dbi[ByteBuffer],
                  val dbWaitingContinuations: Dbi[ByteBuffer],
                  val dbData: Dbi[ByteBuffer],
                  val dbJoins: Dbi[ByteBuffer]) {

  def close(): Unit = {
    dbKeys.close()
    dbData.close()
    dbWaitingContinuations.close()
    dbJoins.close()
    env.close()
  }
}

object LMDBContext {
  private[this] val keysTableName: String                 = "Keys"
  private[this] val waitingContinuationsTableName: String = "WaitingContinuations"
  private[this] val dataTableName: String                 = "Data"
  private[this] val joinsTableName: String                = "Joins"

  def apply(path: Path, mapSize: Long): LMDBContext = {
    val env: Env[ByteBuffer] =
      Env.create().setMapSize(mapSize).setMaxDbs(8).open(path.toFile)

    val dbKeys: Dbi[ByteBuffer] = env.openDbi(keysTableName, MDB_CREATE)
    val dbWaitingContinuations: Dbi[ByteBuffer] =
      env.openDbi(waitingContinuationsTableName, MDB_CREATE)
    val dbData: Dbi[ByteBuffer]  = env.openDbi(dataTableName, MDB_CREATE)
    val dbJoins: Dbi[ByteBuffer] = env.openDbi(joinsTableName, MDB_CREATE)

    new LMDBContext(env, dbKeys, dbWaitingContinuations, dbData, dbJoins)
  }
}
