package coop.rchain.shared

import java.util.NoSuchElementException
import scala.concurrent.SyncVar
import scala.util.{Failure, Success, Try}

class SyncLock {
  private val underlying = new SyncVar[Unit]()
  underlying.put(())

  def tryLock(): Boolean = Try(underlying.take(0)) match {
    case Success(_)                         => true
    case Failure(_: NoSuchElementException) => false
    case Failure(other)                     => throw other
  }

  def unlock(): Unit = underlying.put(())
}
