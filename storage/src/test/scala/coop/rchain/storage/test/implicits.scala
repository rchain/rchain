package coop.rchain.storage.test

import java.nio.charset.StandardCharsets

import coop.rchain.models.Serialize
import coop.rchain.storage.{Match, Transaction}

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

  implicit object dummyTransactionTransaction extends Transaction[DummyTransaction] {

    def commit(t: DummyTransaction): Unit = ()

    def abort(t: DummyTransaction): Unit = ()

    def close(t: DummyTransaction): Unit = ()
  }
}
