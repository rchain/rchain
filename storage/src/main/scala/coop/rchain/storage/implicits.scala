package coop.rchain.storage

import coop.rchain.storage.datamodels.BytesList
import coop.rchain.storage.Serialize.mkProtobufInstance

object implicits {

  implicit val bytesListInstance: Serialize[BytesList] = mkProtobufInstance(BytesList)
}
