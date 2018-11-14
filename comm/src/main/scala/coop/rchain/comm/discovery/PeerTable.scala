package coop.rchain.comm.discovery

import coop.rchain.comm.{CommError, PeerNode}, CommError._
import scala.collection.mutable
import scala.annotation.tailrec
import cats._, cats.data._, cats.implicits._
import coop.rchain.catscontrib._, Catscontrib._

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

  def apply[A <: PeerNode](
      key: Seq[Byte],
      k: Int = PeerTable.Redundancy,
      alpha: Int = PeerTable.Alpha
  ): PeerTable[A] =
    new PeerTable[A](key, k, alpha)

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
final class PeerTable[A <: PeerNode](
    localKey: Seq[Byte],
    private[discovery] val k: Int,
    alpha: Int
) {

  private[discovery] type Entry = PeerTableEntry[A]

  private[discovery] val width = localKey.size // in bytes
  private[discovery] val table = Array.fill(8 * width) {
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
  private[discovery] def distance(a: Seq[Byte], b: Seq[Byte]): Option[Int] = {
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

  private[discovery] def distance(otherKey: Seq[Byte]): Option[Int] = distance(localKey, otherKey)
  private[discovery] def distance(other: A): Option[Int]            = distance(other.key)

  def updateLastSeen[F[_]: Monad: Capture: KademliaRPC](peer: A): F[Unit] = {

    def bucket: F[Option[mutable.ListBuffer[Entry]]] =
      Capture[F].capture(distance(localKey, peer.key).filter(_ < 8 * width).map(table.apply))

    def addUpdateOrPickOldPeer(ps: mutable.ListBuffer[Entry]): F[Option[Entry]] =
      Capture[F].capture {
        ps synchronized {
          ps.find(_.key == peer.key) match {
            case Some(entry) =>
              ps -= entry
              ps += new Entry(peer, _.key)
              None
            case None =>
              if (ps.size < k) {
                ps += new Entry(peer, _.key)
                None
              } else {
                // ping first (oldest) element that isn't already being
                // pinged. If it responds, move it to back (newest
                // position); if it doesn't respond, remove it and place
                // a in back instead
                ps.find(!_.pinging).map { candidate =>
                  candidate.pinging = true
                  candidate
                }
              }
          }
        }
      }

    def pingAndUpdate(ps: mutable.ListBuffer[Entry], older: Entry): F[Unit] =
      KademliaRPC[F].ping(older.entry).map { response =>
        val winner = if (response) older else new Entry(peer, _.key)
        ps synchronized {
          ps -= older
          ps += winner
          winner.pinging = false
        }
      }

    def upsert: OptionT[F, Unit] =
      for {
        ps    <- OptionT(bucket)
        older <- OptionT(addUpdateOrPickOldPeer(ps))
        _     <- OptionT.liftF(pingAndUpdate(ps, older))
      } yield ()

    upsert.value.void
  }

  /**
    * Remove a peer with the given key.
    */
  def remove(key: Seq[Byte]): Unit =
    distance(localKey, key) match {
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

    distance(localKey, key) match {
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
