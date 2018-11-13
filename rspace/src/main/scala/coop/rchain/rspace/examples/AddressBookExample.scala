package coop.rchain.rspace.examples

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, ObjectInputStream, ObjectOutputStream}
import java.nio.file.{Files, Path}

import cats.Id
import cats.effect.{ContextShift, Sync}
import cats.implicits._
import coop.rchain.catscontrib.effect.implicits._
import coop.rchain.rspace.ISpace.IdISpace
import coop.rchain.rspace._
import coop.rchain.rspace.history.Branch
import coop.rchain.shared.Language.ignore
import coop.rchain.rspace.util._
import scala.concurrent.ExecutionContext
import scodec.bits.ByteVector

import scala.collection.immutable.Seq
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

import scala.concurrent.ExecutionContext.Implicits.global

object AddressBookExample {

  /* Here we define a type for channels */

  case class Channel(name: String)

  /* Ordering for Channel */

  implicit val channelOrdering: Ordering[Channel] =
    (x: Channel, y: Channel) => x.name.compare(y.name)

  /* Here we define a type for data */

  case class Name(first: String, last: String)
  case class Address(street: String, city: String, state: String, zip: String)
  case class Entry(name: Name, address: Address, email: String, phone: String)

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

  class EntriesCaptor extends ((Seq[Entry]) => Unit) with Serializable {

    @transient
    private final lazy val res: ListBuffer[Seq[Entry]] = mutable.ListBuffer.empty[Seq[Entry]]

    final def results: Seq[Seq[Entry]] = res.toList

    final def apply(v1: Seq[Entry]): Unit = ignore(res += v1)

    override def hashCode(): Int =
      res.hashCode() * 37

    override def equals(obj: scala.Any): Boolean = obj match {
      case ec: EntriesCaptor => ec.res == res
      case _                 => false
    }
  }

  object implicits {

    implicit val syncF: Sync[Id] = coop.rchain.catscontrib.effect.implicits.syncId

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
    implicit object matchPatternEntry extends Match[Pattern, Nothing, Entry, Entry] {
      def get(p: Pattern, a: Entry): Either[Nothing, Option[Entry]] =
        p match {
          case NameMatch(last) if a.name.last == last        => Right(Some(a))
          case CityMatch(city) if a.address.city == city     => Right(Some(a))
          case StateMatch(state) if a.address.state == state => Right(Some(a))
          case _                                             => Right(None)
        }

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

    // Here we define a temporary place to put the store's files
    val storePath: Path = Files.createTempDirectory("rspace-address-book-example-")

    // Let's define our store
    val context = Context.create[Channel, Pattern, Entry, Printer](storePath, 1024L * 1024L)

    val space =
      RSpace.create[Id, Channel, Pattern, Nothing, Entry, Entry, Printer](context, Branch.MASTER)

    Console.printf("\nExample One: Let's consume and then produce...\n")

    val cres =
      space
        .consume(
          Seq(Channel("friends")),
          Seq(CityMatch(city = "Crystal Lake")),
          new Printer,
          persist = true
        )
        .right
        .get // it should be fine to do that -- type of left side is Nothing (no invalid states)

    assert(cres.isEmpty)

    val pres1 = space.produce(Channel("friends"), alice, persist = false).right.get
    val pres2 = space.produce(Channel("friends"), bob, persist = false).right.get
    val pres3 = space.produce(Channel("friends"), carol, persist = false).right.get

    assert(pres1.nonEmpty)
    assert(pres2.nonEmpty)
    assert(pres3.isEmpty)

    runKs(Seq(pres1, pres2))

    context.close()
  }

  def exampleTwo(): Unit = {

    // Here we define a temporary place to put the store's files
    val storePath: Path = Files.createTempDirectory("rspace-address-book-example-")

    // Let's define our store
    val context = Context.create[Channel, Pattern, Entry, Printer](storePath, 1024L * 1024L)

    val space =
      RSpace.create[Id, Channel, Pattern, Nothing, Entry, Entry, Printer](context, Branch.MASTER)

    Console.printf("\nExample Two: Let's produce and then consume...\n")

    val pres1 = space.produce(Channel("friends"), alice, persist = false).right.get
    val pres2 = space.produce(Channel("friends"), bob, persist = false).right.get
    val pres3 = space.produce(Channel("friends"), carol, persist = false).right.get

    assert(pres1.isEmpty)
    assert(pres2.isEmpty)
    assert(pres3.isEmpty)

    val consumer = () =>
      space
        .consume(
          Seq(Channel("friends")),
          Seq(NameMatch(last = "Lahblah")),
          new Printer,
          persist = false
        )
        .right
        .get

    val cres1 = consumer()
    val cres2 = consumer()
    val cres3 = consumer()

    assert(cres1.isDefined)
    assert(cres2.isDefined)
    assert(cres3.isEmpty)

    runKs(Seq(cres1, cres2))

    Console.printf(space.store.toMap.toString())

    context.close()
  }

  def rollbackExample(): Unit = withSpace { space =>
    println("Rollback example: Let's consume...")

    val cres =
      space
        .consume(
          Seq(Channel("friends")),
          Seq(CityMatch(city = "Crystal Lake")),
          new Printer,
          persist = false
        )
        .right
        .get

    assert(cres.isEmpty)

    println("Rollback example: And create a checkpoint...")
    val checkpointHash = space.createCheckpoint().root

    def produceAlice(): Option[(Printer, Seq[Entry], Int)] =
      space.produce(Channel("friends"), alice, persist = false).right.get

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

    space.close()
  }

  private[this] def withSpace(
      f: IdISpace[Channel, Pattern, Nothing, Entry, Entry, Printer] => Unit
  ) = {
    // Here we define a temporary place to put the store's files
    val storePath = Files.createTempDirectory("rspace-address-book-example-")
    // Let's define our store
    val context = Context.create[Channel, Pattern, Entry, Printer](storePath, 1024L * 1024L)
    val space =
      RSpace.create[Id, Channel, Pattern, Nothing, Entry, Entry, Printer](context, Branch.MASTER)
    try {
      f(space)
    } finally {
      space.close()
      context.close()
    }

  }

}
