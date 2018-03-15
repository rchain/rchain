package coop.rchain.storage.test

import java.nio.charset.StandardCharsets
import java.util.UUID

import coop.rchain.models.Serialize
import coop.rchain.storage.Match

import scala.collection.mutable

object implicits {

  implicit object stringMatch extends Match[Pattern, String] {
    def get(p: Pattern, a: String): Option[String] = Some(a).filter(p.isMatch)
  }

  implicit object stringSerialize extends Serialize[String] {

    def encode(a: String): Array[Byte] =
      a.getBytes(StandardCharsets.UTF_8)

    def decode(bytes: Array[Byte]): Either[Throwable, String] =
      Right(new String(bytes, StandardCharsets.UTF_8))
  }

  implicit object patternSerializeWrapper    extends TestSerialize[Pattern]
  implicit object stringListSerializeWrapper extends TestSerialize[List[String] => Unit]

  class TestSerialize[T] extends Serialize[T] {
    val actionsCache: mutable.Map[UUID, T] = mutable.Map()

    def encode(a: T): Array[Byte] = {
      val uid = UUID.randomUUID()
      actionsCache += uid -> a
      uid.toString.getBytes(StandardCharsets.UTF_8)
    }

    def decode(bytes: Array[Byte]): Either[Throwable, T] = {
      val uidStr = new String(bytes, StandardCharsets.UTF_8)
      val uid    = UUID.fromString(uidStr)
      Right(actionsCache(uid))
    }
  }
}
