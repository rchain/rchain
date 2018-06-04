package coop.rchain.kademlia

import coop.rchain.comm.PeerNode
import scala.collection.mutable

import scala.annotation.tailrec

import cats._, cats.data._, cats.implicits._

import coop.rchain.catscontrib.Capture
import coop.rchain.p2p.effects.Ping

trait Keyed {
  def key: Seq[Byte]
}

trait Peer extends Keyed

trait Latent {
  def latency: Double
}

trait Reputable {
  def reputation: Double
}

object LatencyOrder extends Ordering[Latent] {
  def compare(a: Latent, b: Latent) = a.latency compare b.latency
}

object ReputationOrder extends Ordering[Reputable] {
  def compare(a: Reputable, b: Reputable) = a.reputation compare b.reputation
}

final case class PeerTableEntry[A](entry: A, gkey: A => Seq[Byte]) extends Keyed {
  var pinging           = false
  val key               = gkey(entry)
  override def toString = s"#{PeerTableEntry $entry}"
}

object PeerTable {

  def apply[A <: PeerNode](home: A,
                           k: Int = PeerTable.Redundancy,
                           alpha: Int = PeerTable.Alpha): PeerTable[A] =
    new PeerTable[A](home, k, alpha)

  // Number of bits considered in the distance function. Taken from the
  // passed-in "home" value to the table.
  //
  // val Width = 256

  // Maximum length of each row of the routing table.
  val Redundancy = 20

  // Concurrency factor: system allows up to alpha outstanding network
  // requests at a time.
  val Alpha = 3

  // UNIMPLEMENTED: this parameter controls an optimization that can
  // reduce the number hops required to find an address in the network
  // by grouping keys in buckets of a size larger than one.
  //
  // val BucketWidth = 1

  // Lookup table for log 2 (highest bit set) of an 8-bit unsigned
  // integer. Entry 0 is unused.
  private val dlut = (Seq(0, 7, 6, 6) ++
    Seq.fill(4)(5) ++
    Seq.fill(8)(4) ++
    Seq.fill(16)(3) ++
    Seq.fill(32)(2) ++
    Seq.fill(64)(1) ++
    Seq.fill(128)(0)).toArray
}

/** `PeerTable` implements the routing table used in the Kademlia
  * network discovery and routing protocol.
  *
  */
final class PeerTable[A <: PeerNode](home: A, private[kademlia] val k: Int, alpha: Int) {

  private[kademlia] type Entry = PeerTableEntry[A]

  private[kademlia] val width = home.key.size // in bytes
  private[kademlia] val table = Array.fill(8 * width) {
    new mutable.ListBuffer[Entry]
  }

  /** Computes Kademlia XOR distance.
    *
    * Returns the length of the longest common prefix in bits between
    * the two sequences `a` and `b`. As in Ethereum's implementation,
    * "closer" nodes have higher distance values.
    *
    * @return `Some(Int)` if `a` and `b` are comparable in this table,
    * `None` otherwise.
    */
  private[kademlia] def distance(a: Seq[Byte], b: Seq[Byte]): Option[Int] = {
    @tailrec
    def highBit(idx: Int): Int =
      if (idx == width) 8 * width
      else
        a(idx) ^ b(idx) match {
          case 0 => highBit(idx + 1)
          case n => 8 * idx + PeerTable.dlut(n & 0xff)
        }

    if (a.size != width || b.size != width) None
    else Some(highBit(0))
  }

  private[kademlia] def distance(otherKey: Seq[Byte]): Option[Int] = distance(home.key, otherKey)
  private[kademlia] def distance(other: A): Option[Int]            = distance(other.key)

  private def ping[F[_]: Functor: Ping](ps: mutable.ListBuffer[Entry],
                                        older: Entry,
                                        newer: A): F[Unit] =
    Ping[F].ping(older.entry).map { response =>
      val winner = if (response) older else new Entry(newer, _.key)
      ps synchronized {
        ps -= older
        ps += winner
        winner.pinging = false
      }
    }

  /** Update the last-seen time of `peer`, possibly adding it to the
    * routing table.
    *
    * If `add` is true, and `peer` is not in the table, it is
    * added. If the table was already full at that distance, test the
    * least recently seen peer at that distance. If that peer
    * responds, discard `peer`; otherwise, discard the older entry and
    * insert `peer`.
    *
    * If `peer` is already in the table, it becomes the most recently
    * seen entry at its distance.
    */
  def observe[F[_]: Applicative: Capture: Ping](peer: A, add: Boolean): F[Unit] =
    distance(home.key, peer.key) match {
      case Some(index) =>
        if (index < 8 * width) {
          val ps = table(index)
          ps synchronized {
            ps.find(_.key == peer.key) match {
              case Some(entry) =>
                Capture[F].capture {
                  ps -= entry
                  ps += entry
                }
              case None if add =>
                if (ps.size < k) {
                  Capture[F].capture(ps += new Entry(peer, _.key))
                } else {
                  // ping first (oldest) element that isn't already being
                  // pinged. If it responds, move it to back (newest
                  // position); if it doesn't respond, remove it and place
                  // a in back instead
                  ps.find(!_.pinging)
                    .map { candidate =>
                      candidate.pinging = true
                      ping[F](ps, candidate, peer)
                    }
                    .getOrElse(().pure[F])
                }
              case None => ().pure[F]
            }
          }
        } else ().pure[F]
      case None => ().pure[F]
    }

  /**
    * Remove a peer with the given key.
    */
  def remove(key: Seq[Byte]): Unit =
    distance(home.key, key) match {
      case Some(index) =>
        if (index < 8 * width) {
          val ps = table(index)
          ps synchronized {
            ps.find(_.key == key) match {
              case Some(entry) => {
                ps -= entry
                ()
              }
              case _ => ()
            }
          }
        }
      case None => ()
    }

  /**
    * Return the `k` nodes closest to `key` that this table knows
    * about, sorted ascending by distance to `key`.
    */
  def lookup(key: Seq[Byte]): Seq[A] = {

    def sorter(a: A, b: A) =
      (distance(key, a.key), distance(key, b.key)) match {
        case (Some(d0), Some(d1)) => d0 > d1
        case _                    => false
      }

    distance(home.key, key) match {
      case Some(index) => {
        val entries = new mutable.ListBuffer[Entry]

        for (i <- index to 8 * width - 1; if entries.size < k) {
          table(i) synchronized {
            entries ++= table(i).filter(_.entry.key != key)
          }
        }

        for (i <- index - 1 to 0 by -1; if entries.size < k) {
          table(i) synchronized {
            entries ++= table(i)
          }
        }

        entries.map(_.entry).sortWith(sorter).take(k).toVector
      }
      case None => Vector.empty
    }
  }

  /**
    * Return `Some[A]` if `key` names an entry in the table.
    */
  def find(key: Seq[Byte]): Option[A] =
    for {
      d <- distance(key)
      e <- table(d) synchronized { table(d).find(_.entry.key == key) }
    } yield e.entry

  /**
    * Return a sequence of all the `A`s in the table.
    */
  def peers: Seq[A] =
    table.flatMap(l => l synchronized { l.map(_.entry) })

  /**
    * Return all distances in order from least to most filled.
    *
    * Optionally, ignore any distance closer than [[limit]].
    */
  def sparseness(limit: Int = 255): Seq[Int] =
    table
      .take(limit + 1)
      .zipWithIndex
      .map { case (l, i) => (l.size, i) }
      .sortWith(_._1 < _._1)
      .map(_._2)
}
