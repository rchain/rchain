package coop.rchain.rspace.examples

import cats.effect.{Concurrent, ContextShift}
import cats.{Applicative, Id}
import coop.rchain.crypto.codec.Base16
import coop.rchain.metrics.{Metrics, NoopSpan, Span}
import coop.rchain.rspace.hashing.Blake2b256Hash
import coop.rchain.rspace.{RSpace, _}
import coop.rchain.rspace.syntax.rspaceSyntaxKeyValueStoreManager
import coop.rchain.rspace.util._
import coop.rchain.shared.Language.ignore
import coop.rchain.shared.{Log, Serialize}
import coop.rchain.store.InMemoryStoreManager
import scodec.bits.ByteVector

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, ObjectInputStream, ObjectOutputStream}
import scala.collection.mutable.ListBuffer
import scala.concurrent.ExecutionContext
import scala.concurrent.ExecutionContext.Implicits.global

@SuppressWarnings(Array("org.wartremover.warts.EitherProjectionPartial"))
object AddressBookExample {

//  /* Here we define a type for channels */
//
//  final case class Channel(name: String)
//
//  /* Ordering for Channel */
//
//  implicit val channelOrdering: Ordering[Channel] =
//    (x: Channel, y: Channel) => x.name.compare(y.name)

  /* Here we define a type for data */

  final case class Name(first: String, last: String)
  final case class Address(street: String, city: String, state: String, zip: String)
  final case class Entry(name: Name, address: Address, email: String, phone: String)

  /* Here we define a type for patterns */

  sealed trait Pattern                       extends Product with Serializable
  final case class NameMatch(last: String)   extends Pattern
  final case class CityMatch(city: String)   extends Pattern
  final case class StateMatch(state: String) extends Pattern

  /* Here we define a type for continuations */

  class Printer extends ((Seq[Entry]) => Unit) with Serializable {

    def apply(entries: Seq[Entry]): Unit =
      entries.foreach {
        case Entry(name, address, email, phone) =>
          val nameStr = s"${name.last}, ${name.first}"
          val addrStr = s"${address.street}, ${address.city}, ${address.state} ${address.zip}"
          Console.printf(s"""|
                             |=== ENTRY ===
                             |name:    $nameStr
                             |address: $addrStr
                             |email:   $email
                             |phone:   $phone
                             |""".stripMargin)
      }
  }

  @SuppressWarnings(Array("org.wartremover.warts.MutableDataStructures"))
  class EntriesCaptor extends ((Seq[Entry]) => Unit) with Serializable {

    @transient
    private final lazy val res: ListBuffer[Seq[Entry]] = ListBuffer.empty[Seq[Entry]]

    final def results: Seq[Seq[Entry]] = res.toList

    final def apply(v1: Seq[Entry]): Unit = ignore(res += v1)

    override def hashCode(): Int =
      res.hashCode() * 37

    override def equals(obj: scala.Any): Boolean = obj match {
      case ec: EntriesCaptor => ec.res == res
      case _                 => false
    }
  }

  def mkChannel(name: String) = {
    val bytes = ByteVector.encodeUtf8(name).right.get
    Channel(Blake2b256Hash.create(bytes))
  }

  object implicits {

    implicit val concurrentF: Concurrent[Id] = coop.rchain.catscontrib.effect.implicits.concurrentId

    implicit val contextShiftId: ContextShift[Id] =
      new ContextShift[Id] {
        def shift: Id[Unit]                                   = ???
        def evalOn[A](ec: ExecutionContext)(fa: Id[A]): Id[A] = fa
      }

    /* Now I will troll Greg... */

    /* Serialize instances */

    /**
      * An instance of [[Serialize]] for [[Channel]]
      */
    implicit val serializeChannel: Serialize[Channel] = new Serialize[Channel] {

      def encode(channel: Channel): ByteVector = {
        val baos = new ByteArrayOutputStream()
        try {
          val oos = new ObjectOutputStream(baos)
          try {
            oos.writeObject(channel)
          } finally {
            oos.close()
          }
          ByteVector.view(baos.toByteArray)
        } finally {
          baos.close()
        }
      }

      @SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf"))
      def decode(bytes: ByteVector): Either[Throwable, Channel] =
        try {
          val bais = new ByteArrayInputStream(bytes.toArray)
          try {
            val ois = new ObjectInputStream(bais)
            try {
              Right(ois.readObject.asInstanceOf[Channel])
            } finally {
              ois.close()
            }
          } finally {
            bais.close()
          }
        } catch {
          case ex: Throwable => Left(ex)
        }
    }

    /**
      * An instance of [[Serialize]] for [[Pattern]]
      */
    implicit val serializePattern: Serialize[Pattern] = makeSerializeFromSerializable[Pattern]

    /**
      * An instance of [[Serialize]] for [[Entry]]
      */
    implicit val serializeInfo: Serialize[Entry] = makeSerializeFromSerializable[Entry]

    /**
      * An instance of [[Serialize]] for [[Printer]]
      */
    implicit val serializePrinter: Serialize[Printer] = makeSerializeFromSerializable[Printer]

    /**
      * An instance of [[Serialize]] for [[EntriesCaptor]]
      */
    implicit val serializeEntriesCaptor: Serialize[EntriesCaptor] =
      makeSerializeFromSerializable[EntriesCaptor]

    /* Match instance */

    /**
      * An instance of [[Match]] for [[Pattern]] and [[Entry]]
      */
    implicit def matchPatternEntry[F[_]](
        implicit apF: Applicative[F]
    ): Match[F, Pattern, Entry] =
      (p: Pattern, a: Entry) =>
        p match {
          case NameMatch(last) if a.name.last == last        => apF.pure(Some(a))
          case CityMatch(city) if a.address.city == city     => apF.pure(Some(a))
          case StateMatch(state) if a.address.state == state => apF.pure(Some(a))
          case _                                             => apF.pure(None)
        }
  }

  import implicits._

  // Let's define some Entries
  val alice = Entry(
    name = Name("Alice", "Lincoln"),
    address = Address("777 Ford St.", "Crystal Lake", "Idaho", "223322"),
    email = "alicel@ringworld.net",
    phone = "787-555-1212"
  )

  val bob = Entry(
    name = Name("Bob", "Lahblah"),
    address = Address("1000 Main St", "Crystal Lake", "Idaho", "223322"),
    email = "blablah@tenex.net",
    phone = "698-555-1212"
  )

  val carol = Entry(
    name = Name("Carol", "Lahblah"),
    address = Address("22 Goldwater Way", "Herbert", "Nevada", "334433"),
    email = "carol@blablah.org",
    phone = "232-555-1212"
  )

  def exampleOne(): Unit = {

    implicit val log: Log[Id]          = Log.log
    implicit val metricsF: Metrics[Id] = new Metrics.MetricsNOP[Id]()
    implicit val spanF: Span[Id]       = NoopSpan[Id]()
    implicit val keyValueStoreManager  = InMemoryStoreManager[Id]

    // Let's define our store
    val store = keyValueStoreManager.rSpaceStores
    val space = RSpace.create[Id, Pattern, Entry, Printer](store)

    Console.printf("\nExample One: Let's consume and then produce...\n")

    val cres =
      space
        .consume(
          Seq(mkChannel("friends")),
          Seq(CityMatch(city = "Crystal Lake")),
          new Printer,
          persist = true
        ) // it should be fine to do that -- type of left side is Nothing (no invalid states)

    assert(cres.isEmpty)

    val pres1 = space.produce(mkChannel("friends"), alice, persist = false)
    val pres2 = space.produce(mkChannel("friends"), bob, persist = false)
    val pres3 = space.produce(mkChannel("friends"), carol, persist = false)

    assert(pres1.nonEmpty)
    assert(pres2.nonEmpty)
    assert(pres3.isEmpty)

    runKs(Seq(pres1, pres2))
  }

  def exampleTwo(): Unit = {

    implicit val log: Log[Id]          = Log.log
    implicit val metricsF: Metrics[Id] = new Metrics.MetricsNOP[Id]()
    implicit val spanF: Span[Id]       = NoopSpan[Id]()
    implicit val keyValueStoreManager  = InMemoryStoreManager[Id]

    // Let's define our store
    val store = keyValueStoreManager.rSpaceStores
    val space = RSpace.create[Id, Pattern, Entry, Printer](store)

    Console.printf("\nExample Two: Let's produce and then consume...\n")

    val pres1 = space.produce(mkChannel("friends"), alice, persist = false)
    val pres2 = space.produce(mkChannel("friends"), bob, persist = false)
    val pres3 = space.produce(mkChannel("friends"), carol, persist = false)

    assert(pres1.isEmpty)
    assert(pres2.isEmpty)
    assert(pres3.isEmpty)

    val consumer = () =>
      space
        .consume(
          Seq(mkChannel("friends")),
          Seq(NameMatch(last = "Lahblah")),
          new Printer,
          persist = false
        )

    val cres1 = consumer()
    val cres2 = consumer()
    val cres3 = consumer()

    assert(cres1.isDefined)
    assert(cres2.isDefined)
    assert(cres3.isEmpty)

    runKs(Seq(cres1, cres2))

    Console.printf(space.toMap.toString())
  }

  def rollbackExample(): Unit = withSpace { space =>
    println("Rollback example: Let's consume...")

    val cres =
      space
        .consume(
          Seq(mkChannel("friends")),
          Seq(CityMatch(city = "Crystal Lake")),
          new Printer,
          persist = false
        )

    assert(cres.isEmpty)

    println("Rollback example: And create a checkpoint...")
    val checkpointHash = space.createCheckpoint().root

    def produceAlice(): Option[(Printer, Seq[Entry])] =
      unpackOption(space.produce(mkChannel("friends"), alice, persist = false))

    println("Rollback example: First produce result should return some data")
    assert(produceAlice.isDefined)

    println("Rollback example: Second produce result should be empty")
    assert(produceAlice.isEmpty)

    println("Rollback example: Every following produce result should be empty")
    assert(produceAlice.isEmpty)

    println(
      "Rollback example: Let's reset RSpace to the state from before running the produce operations"
    )
    space.reset(checkpointHash)

    println("Rollback example: Again, first produce result should return some data")
    assert(produceAlice.isDefined)

    println("Rollback example: And again second produce result should be empty")
    assert(produceAlice.isEmpty)

  }

  private[this] def withSpace(
      f: ISpace[Id, Channel, Pattern, Entry, Printer] => Unit
  ) = {

    implicit val log: Log[Id]          = Log.log
    implicit val metricsF: Metrics[Id] = new Metrics.MetricsNOP[Id]()
    implicit val spanF: Span[Id]       = NoopSpan[Id]()
    implicit val keyValueStoreManager  = InMemoryStoreManager[Id]

    // Let's define our store
    val store = keyValueStoreManager.rSpaceStores
    val space = RSpace.create[Id, Pattern, Entry, Printer](store)
    try {
      f(space)
    } finally {
      ()
    }

  }

}
