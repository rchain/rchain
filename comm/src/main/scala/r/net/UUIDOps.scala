package r.net

import java.util.UUID

trait UUIDOps {
  def getUUID(): UUID = UUID.randomUUID()
  def getUUID(uuid: String) = UUID.fromString(uuid)
}