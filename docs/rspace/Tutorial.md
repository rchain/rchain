# An rspace tutorial

### Getting Started


To start using `rspace`, we first import the library into our project.
```scala
scala> import coop.rchain.rspace._
import coop.rchain.rspace._
```

Before we can start using the main `produce` and `consume` functions, we need to define data types for channels, patterns, data, and continuations.

Let's try writing types for a simple address book-style project, where we can store and retrieve information about people.

We start with a simple channel.

```scala
case class Channel(name: String)
```
Our `Channel` type will encode categories of addresses, like "friends", "family", and "colleagues".

Now let's write our types for patterns and data.

For data, we will use a type called `Entry`.
```scala
case class Name(first: String, last: String)
case class Address(street: String, city: String, state: String, zip: String)
case class Entry(name: Name, address: Address, email: String, phone: String)
```

Next we will write our pattern type.  We will use values of type `Pattern` for matching with values of type `Entry`.
```scala
sealed trait Pattern extends Product with Serializable
case class NameMatch(last: String) extends Pattern
case class CityMatch(city: String) extends Pattern
case class StateMatch(state: String) extends Pattern
```

### Defining a Continuation Type

In `rspace`, a continuation can be an arbitrary type just like our channels, patterns, and data (with some constraints, see below).  We don't actually run the continuations in `rspace` - that is the responsibility of the caller.

However, the library was designed to be used in a context where continuations take produced data as their input.

For our purposes we will design a continuation type as a Scala class which implements `Function1`, so that it can be easily called like a function.

Our continuation will be a simple one which prints the entries it is given.

```scala
class Printer extends ((List[Entry]) => Unit) with Serializable {

  def apply(entries: List[Entry]): Unit =
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
```

As a convenience, we've provided definitions for these types in the `AddressBookExample` object in `examples` package.

```scala
scala> import coop.rchain.rspace.examples.AddressBookExample._
import coop.rchain.rspace.examples.AddressBookExample._
```

### Creating `Serialize` Instances

`rspace` uses a *type class*-based API.  The API places simple constraints what Scala data types we can use with `rspace`.

If you are unfamiliar with type classes, the [Cats documentation](https://typelevel.org/cats/typeclasses.html) is a great place to start learning about them.

In order to use our types with `rspace`, we must provide instances of `rspace`'s `Serialize` type class for each type.

Here is the definition of the `Serialize` type class:

```scala
/**
  * Type class for serializing and deserializing values
  *
  * @tparam A The type to serialize and deserialize.
  */
trait Serialize[A] {

  def encode(a: A): ByteVector

  def decode(bytes: ByteVector): Either[Throwable, A]
}
```

Let's try defining an instance of `Serialize` for `Channel` using Java serialization.

First we will need to import some more stuff.
```scala
scala> import java.io.{ByteArrayInputStream, ByteArrayOutputStream, ObjectInputStream, ObjectOutputStream}
import java.io.{ByteArrayInputStream, ByteArrayOutputStream, ObjectInputStream, ObjectOutputStream}

scala> import scodec.bits.ByteVector
import scodec.bits.ByteVector
```

Now we define an instance of `Serialize`.
```scala
scala> implicit object serializeChannel extends Serialize[Channel] {
     | 
     |   def encode(channel: Channel): ByteVector = {
     |     val baos = new ByteArrayOutputStream()
     |     try {
     |       val oos = new ObjectOutputStream(baos)
     |       try { oos.writeObject(channel) } finally { oos.close() }
     |       ByteVector.view(baos.toByteArray)
     |     } finally {
     |       baos.close()
     |     }
     |   }
     | 
     |   def decode(bytes: ByteVector): Either[Throwable, Channel] = {
     |     try {
     |       val bais = new ByteArrayInputStream(bytes.toArray)
     |       try {
     |         val ois = new ObjectInputStream(bais)
     |         try { Right(ois.readObject.asInstanceOf[Channel]) } finally { ois.close() }
     |       } finally {
     |         bais.close()
     |       }
     |     } catch {
     |       case ex: Throwable => Left(ex)
     |     }
     |   }
     | }
defined object serializeChannel
```

Let's try using it.
```scala
scala> val chan = new Channel("friends")
chan: coop.rchain.rspace.examples.AddressBookExample.Channel = Channel(friends)

scala> val roundTrippedChan = serializeChannel.decode(serializeChannel.encode(chan))
roundTrippedChan: Either[Throwable,coop.rchain.rspace.examples.AddressBookExample.Channel] = Right(Channel(friends))
```

Looking at the instance we above we could actually write a function to generate these instances for any type `T` that's a subtype of `scala.Serializable`.

In fact, we have provided this convenience function in the `coop.rchain.rspace.examples` package.

```scala
scala> import coop.rchain.rspace.examples._
import coop.rchain.rspace.examples._
```

Let's go ahead use that to make instances for our other types.
```scala
scala> implicit val serializeEntry: Serialize[Entry] = makeSerializeFromSerializable[Entry]
serializeEntry: coop.rchain.rspace.Serialize[coop.rchain.rspace.examples.AddressBookExample.Entry] = coop.rchain.rspace.examples.package$$anon$1@77f4ba3

scala> implicit val serializePattern: Serialize[Pattern] = makeSerializeFromSerializable[Pattern]
serializePattern: coop.rchain.rspace.Serialize[coop.rchain.rspace.examples.AddressBookExample.Pattern] = coop.rchain.rspace.examples.package$$anon$1@62567af

scala> implicit val serializePrinter: Serialize[Printer] = makeSerializeFromSerializable[Printer]
serializePrinter: coop.rchain.rspace.Serialize[coop.rchain.rspace.examples.AddressBookExample.Printer] = coop.rchain.rspace.examples.package$$anon$1@24a9babf
```

Now we will define some example `Entry`s.
```scala
scala> val alice = Entry(name = Name("Alice", "Lincoln"),
     |                   address = Address("777 Ford St.", "Crystal Lake", "Idaho", "223322"),
     |                   email = "alicel@ringworld.net",
     |                   phone = "777-555-1212")
alice: coop.rchain.rspace.examples.AddressBookExample.Entry = Entry(Name(Alice,Lincoln),Address(777 Ford St.,Crystal Lake,Idaho,223322),alicel@ringworld.net,777-555-1212)

scala> val bob = Entry(name = Name("Bob", "Lahblah"),
     |                 address = Address("1000 Main St", "Crystal Lake", "Idaho", "223322"),
     |                 email = "blablah@tenex.net",
     |                 phone = "698-555-1212")
bob: coop.rchain.rspace.examples.AddressBookExample.Entry = Entry(Name(Bob,Lahblah),Address(1000 Main St,Crystal Lake,Idaho,223322),blablah@tenex.net,698-555-1212)

scala> val carol = Entry(name = Name("Carol", "Lahblah"),
     |                   address = Address("22 Goldwater Way", "Herbert", "Nevada", "334433"),
     |                   email = "carol@blablah.org",
     |                   phone = "232-555-1212")
carol: coop.rchain.rspace.examples.AddressBookExample.Entry = Entry(Name(Carol,Lahblah),Address(22 Goldwater Way,Herbert,Nevada,334433),carol@blablah.org,232-555-1212)
```

Let's test out one of our other new `Serialize` instances with one of these values:
```scala
scala> val roundTrippedAlice = serializeEntry.decode(serializeEntry.encode(alice))
roundTrippedAlice: Either[Throwable,coop.rchain.rspace.examples.AddressBookExample.Entry] = Right(Entry(Name(Alice,Lincoln),Address(777 Ford St.,Crystal Lake,Idaho,223322),alicel@ringworld.net,777-555-1212))
```

### Creating a `Match` instance

In addition to providing instances of the `Serialize` type classe for our chosen data types, we also must provide an instance of the `Match` type class.  This instance is used to match values of our `Pattern` type with values of our `Entry` type.

Here is the definition of the `Match` type class.
```scala
/**
  * Type class for matching patterns with data.
  *
  * @tparam P A type representing patterns
  * @tparam E A type representing illegal state
  * @tparam A A type representing data
  * @tparam R A type representing a match result
  */
trait Match[P, E, A, R] {

  def get(p: P, a: A): Either[E, Option[R]]
}
```

Let's try defining an instance of `Match` for `Pattern` and `Entry`.
```scala
scala> implicit object matchPatternEntry extends Match[Pattern, Nothing, Entry, Entry] {
     |   def get(p: Pattern, a: Entry): Either[Nothing, Option[Entry]] =
     |     p match {
     |       case NameMatch(last) if a.name.last == last        => Right(Some(a))
     |       case CityMatch(city) if a.address.city == city     => Right(Some(a))
     |       case StateMatch(state) if a.address.state == state => Right(Some(a))
     |       case _                                             => Right(None)
     |     }
     | }
defined object matchPatternEntry
```

Let's see this instance in action.
```scala
scala> val mat = matchPatternEntry.get(NameMatch("Lincoln"), alice)
mat: Either[Nothing,Option[coop.rchain.rspace.examples.AddressBookExample.Entry]] = Right(Some(Entry(Name(Alice,Lincoln),Address(777 Ford St.,Crystal Lake,Idaho,223322),alicel@ringworld.net,777-555-1212)))

scala> val noMat = matchPatternEntry.get(NameMatch("Lincoln"), bob)
noMat: Either[Nothing,Option[coop.rchain.rspace.examples.AddressBookExample.Entry]] = Right(None)
```

### Instantiating the Store

First we define a temporary place to put the store's files.
```scala
scala> import java.nio.file.{Files, Path}
import java.nio.file.{Files, Path}

scala> val storePath: Path = Files.createTempDirectory("rspace-address-book-example-")
storePath: java.nio.file.Path = /var/folders/rk/kf3ndvw96bnd18fcrtf1vjfh0000gn/T/rspace-address-book-example-5156619535042951652
```

Next we create an instance of `Context` using `storePath`.  We will create our store with a maximum map size of 100MB.
```scala
scala> import cats.Id
import cats.Id

scala> import cats.effect.Sync
import cats.effect.Sync

scala> implicit val syncF: Sync[Id] = coop.rchain.catscontrib.effect.implicits.syncId
syncF: cats.effect.Sync[cats.Id] = coop.rchain.catscontrib.effect.implicits.package$$anon$1@3ba63c94

scala> val context: Context[Channel, Pattern, Entry, Printer] = Context.create[Channel, Pattern, Entry, Printer](storePath, 1024L * 1024L * 100L)
context: coop.rchain.rspace.Context[coop.rchain.rspace.examples.AddressBookExample.Channel,coop.rchain.rspace.examples.AddressBookExample.Pattern,coop.rchain.rspace.examples.AddressBookExample.Entry,coop.rchain.rspace.examples.AddressBookExample.Printer] = coop.rchain.rspace.LMDBContext@7409b43c
```
Now we can create an RSpace using the created context
```scala
scala> val space = RSpace.create[Id, Channel, Pattern, Nothing, Entry, Entry, Printer](context, coop.rchain.rspace.history.Branch.MASTER)
space: cats.Id[coop.rchain.rspace.ISpace[cats.Id,coop.rchain.rspace.examples.AddressBookExample.Channel,coop.rchain.rspace.examples.AddressBookExample.Pattern,Nothing,coop.rchain.rspace.examples.AddressBookExample.Entry,coop.rchain.rspace.examples.AddressBookExample.Entry,coop.rchain.rspace.examples.AddressBookExample.Printer]] = coop.rchain.rspace.spaces.CoarseGrainedRSpace@7b593ab1
```

### Producing and Consuming

First we will install a "query" in the store using `consume`.
```scala
scala> val cres1 = space.consume(List(Channel("friends")), List(CityMatch(city = "Crystal Lake")), new Printer, persist = false)
cres1: cats.Id[Either[Nothing,Option[(coop.rchain.rspace.examples.AddressBookExample.Printer, scala.collection.immutable.Seq[coop.rchain.rspace.examples.AddressBookExample.Entry])]]] = Right(None)
```

Here we are installing a continuation in the store at the "friends" `Channel`.  This continuation will be returned to us when a piece of matching data is introduced to the store at that channel.

Now let's try introducing a piece of data to the space using `produce`.
```scala
scala> val pres1 = space.produce(Channel("friends"), alice, persist = false)
pres1: cats.Id[Either[Nothing,Option[(coop.rchain.rspace.examples.AddressBookExample.Printer, scala.collection.immutable.Seq[coop.rchain.rspace.examples.AddressBookExample.Entry])]]] = Right(Some((<function1>,List(Entry(Name(Alice,Lincoln),Address(777 Ford St.,Crystal Lake,Idaho,223322),alicel@ringworld.net,777-555-1212)))))
```

If we look closely at the result, we can see that we have received back an `Option` containing a `<function1>` along with the data we passed to `produce`.  This is because this data has satisfied the match with the `Pattern` we provided in our call to `consume`.

Let's run the continuation using a function from the `util` package.
```scala
scala> import coop.rchain.rspace.util._
import coop.rchain.rspace.util._

scala> runK(pres1)

=== ENTRY ===
name:    Lincoln, Alice
address: 777 Ford St., Crystal Lake, Idaho 223322
email:   alicel@ringworld.net
phone:   777-555-1212
```

When we inspect the contents of the store, we notice that the store is empty.  The continuation has been removed.  The data was also not stored because a matching continuation was found.
```scala
scala> println(space.store.toMap)
Map()
```

Let's reinstall the same continuation.
```scala
scala> val cres2 = space.consume(List(Channel("friends")), List(CityMatch(city = "Crystal Lake")), new Printer, persist = false)
cres2: cats.Id[Either[Nothing,Option[(coop.rchain.rspace.examples.AddressBookExample.Printer, scala.collection.immutable.Seq[coop.rchain.rspace.examples.AddressBookExample.Entry])]]] = Right(None)
```

Now let's try introducing another piece of data to the space.
```scala
scala> val pres2 = space.produce(Channel("friends"), bob, persist = false)
pres2: cats.Id[Either[Nothing,Option[(coop.rchain.rspace.examples.AddressBookExample.Printer, scala.collection.immutable.Seq[coop.rchain.rspace.examples.AddressBookExample.Entry])]]] = Right(Some((<function1>,List(Entry(Name(Bob,Lahblah),Address(1000 Main St,Crystal Lake,Idaho,223322),blablah@tenex.net,698-555-1212)))))
```

Let's also run the continuation and inspect the store.
```scala
scala> runK(pres2)

=== ENTRY ===
name:    Lahblah, Bob
address: 1000 Main St, Crystal Lake, Idaho 223322
email:   blablah@tenex.net
phone:   698-555-1212

scala> println(space.store.toMap)
Map()
```

Now let's reinstall the the continuation and introduce another piece of data to the space.
```scala
scala> val cres3 = space.consume(List(Channel("friends")), List(CityMatch(city = "Crystal Lake")), new Printer, persist = false)
cres3: cats.Id[Either[Nothing,Option[(coop.rchain.rspace.examples.AddressBookExample.Printer, scala.collection.immutable.Seq[coop.rchain.rspace.examples.AddressBookExample.Entry])]]] = Right(None)

scala> val pres3 = space.produce(Channel("friends"), carol, persist = false)
pres3: cats.Id[Either[Nothing,Option[(coop.rchain.rspace.examples.AddressBookExample.Printer, scala.collection.immutable.Seq[coop.rchain.rspace.examples.AddressBookExample.Entry])]]] = Right(None)
```

This time we receive a `None` back from `produce`, indicating that no match has been made.

Let's inspect the contents of the store again.
```scala
scala> println(space.store.toMap)
Map(List(Channel(friends)) -> Row(List(Datum(Entry(Name(Carol,Lahblah),Address(22 Goldwater Way,Herbert,Nevada,334433),carol@blablah.org,232-555-1212),false,Produce(channels: Blake2b256Hash(bytes: ByteVector(32 bytes, 0xeac5571fbf9fc71f19d65e14314b32ad06538a364c2ff101edfa99a969fac35b)), hash: Blake2b256Hash(bytes: ByteVector(32 bytes, 0x7f7df0d04cc2005bde1d5b58498c24296aea8eba00d096e1baa79ee31f342e76))))),List(WaitingContinuation(List(CityMatch(Crystal Lake)),<function1>,false,Consume(channels: Blake2b256Hash(bytes: ByteVector(32 bytes, 0xeac5571fbf9fc71f19d65e14314b32ad06538a364c2ff101edfa99a969fac35b)), hash: Blake2b256Hash(bytes: ByteVector(32 bytes, 0x02e56a04fd6302d086530a4b862541075aafddb91b62e2b2e0e4f9509fd8e1f9)))))))
```

Here we can see that that at `Channel("friends")`, we have now stored Carol's `Entry` record along with our continuations and its patterns.

Let's `consume` again, but this time, let's use a different pattern - one that will match Carol's `Entry` - returning it to us.
```scala
scala> val cres4 = space.consume(List(Channel("friends")), List(NameMatch(last = "Lahblah")), new Printer, persist = false)
cres4: cats.Id[Either[Nothing,Option[(coop.rchain.rspace.examples.AddressBookExample.Printer, scala.collection.immutable.Seq[coop.rchain.rspace.examples.AddressBookExample.Entry])]]] = Right(Some((<function1>,List(Entry(Name(Carol,Lahblah),Address(22 Goldwater Way,Herbert,Nevada,334433),carol@blablah.org,232-555-1212)))))

scala> runK(cres4)

=== ENTRY ===
name:    Lahblah, Carol
address: 22 Goldwater Way, Herbert, Nevada 334433
email:   carol@blablah.org
phone:   232-555-1212
```

Indeed, our call to `consume` has returned another continuation along with Carol's `Entry`.

In the store, we see that there is still a waiting continuation at `Channel("friends")`
```scala
scala> println(space.store.toMap)
Map(List(Channel(friends)) -> Row(List(),List(WaitingContinuation(List(CityMatch(Crystal Lake)),<function1>,false,Consume(channels: Blake2b256Hash(bytes: ByteVector(32 bytes, 0xeac5571fbf9fc71f19d65e14314b32ad06538a364c2ff101edfa99a969fac35b)), hash: Blake2b256Hash(bytes: ByteVector(32 bytes, 0x02e56a04fd6302d086530a4b862541075aafddb91b62e2b2e0e4f9509fd8e1f9)))))))
```

Let's produce one more time to let this sink in.
```scala
scala> val pres4 = space.produce(Channel("friends"), alice, persist = false)
pres4: cats.Id[Either[Nothing,Option[(coop.rchain.rspace.examples.AddressBookExample.Printer, scala.collection.immutable.Seq[coop.rchain.rspace.examples.AddressBookExample.Entry])]]] = Right(Some((<function1>,List(Entry(Name(Alice,Lincoln),Address(777 Ford St.,Crystal Lake,Idaho,223322),alicel@ringworld.net,777-555-1212)))))

scala> runK(pres4)

=== ENTRY ===
name:    Lincoln, Alice
address: 777 Ford St., Crystal Lake, Idaho 223322
email:   alicel@ringworld.net
phone:   777-555-1212

scala> println(space.store.toMap)
Map()
```


### Using Multiple Channels

Now we are going to experiment with multiple channels.  Let's first define a few more `Entry` values - one for our colleague Dan and another for our friend Erin.
```scala
scala> val dan = Entry(name = Name("Dan", "Walters"),
     |                 address = Address("40 Shady Lane", "Crystal Lake", "Idaho", "223322"),
     |                 email = "deejwalters@sdf.lonestar.org",
     |                 phone = "444-555-1212")
dan: coop.rchain.rspace.examples.AddressBookExample.Entry = Entry(Name(Dan,Walters),Address(40 Shady Lane,Crystal Lake,Idaho,223322),deejwalters@sdf.lonestar.org,444-555-1212)

scala> val erin = Entry(name = Name("Erin", "Rush"),
     |                  address = Address("23 Market St.", "Peony", "Idaho", "224422"),
     |                  email = "erush@lasttraintogoa.net",
     |                  phone = "333-555-1212")
erin: coop.rchain.rspace.examples.AddressBookExample.Entry = Entry(Name(Erin,Rush),Address(23 Market St.,Peony,Idaho,224422),erush@lasttraintogoa.net,333-555-1212)
```

Let's introduce Dan and Erin to the store with `produce`.
```scala
scala> val pres5 = space.produce(Channel("colleagues"), dan, persist = false)
pres5: cats.Id[Either[Nothing,Option[(coop.rchain.rspace.examples.AddressBookExample.Printer, scala.collection.immutable.Seq[coop.rchain.rspace.examples.AddressBookExample.Entry])]]] = Right(None)

scala> val pres6 = space.produce(Channel("friends"), erin, persist = false)
pres6: cats.Id[Either[Nothing,Option[(coop.rchain.rspace.examples.AddressBookExample.Printer, scala.collection.immutable.Seq[coop.rchain.rspace.examples.AddressBookExample.Entry])]]] = Right(None)
```

Now let's `consume` on multiple channels, searching for our friends and colleagues who live in Idaho.
```scala
scala> val cres5 = space.consume(List(Channel("friends"), Channel("colleagues")), List(StateMatch("Idaho"), StateMatch("Idaho")), new Printer, persist = false)
cres5: cats.Id[Either[Nothing,Option[(coop.rchain.rspace.examples.AddressBookExample.Printer, scala.collection.immutable.Seq[coop.rchain.rspace.examples.AddressBookExample.Entry])]]] = Right(Some((<function1>,List(Entry(Name(Erin,Rush),Address(23 Market St.,Peony,Idaho,224422),erush@lasttraintogoa.net,333-555-1212), Entry(Name(Dan,Walters),Address(40 Shady Lane,Crystal Lake,Idaho,223322),deejwalters@sdf.lonestar.org,444-555-1212)))))

scala> runK(cres5)

=== ENTRY ===
name:    Rush, Erin
address: 23 Market St., Peony, Idaho 224422
email:   erush@lasttraintogoa.net
phone:   333-555-1212

=== ENTRY ===
name:    Walters, Dan
address: 40 Shady Lane, Crystal Lake, Idaho 223322
email:   deejwalters@sdf.lonestar.org
phone:   444-555-1212
```

Now let's do the same thing in the opposite order.
```scala
scala> val cres6 = space.consume(List(Channel("friends"), Channel("colleagues")), List(StateMatch("Idaho"), StateMatch("Idaho")), new Printer, persist = false)
cres6: cats.Id[Either[Nothing,Option[(coop.rchain.rspace.examples.AddressBookExample.Printer, scala.collection.immutable.Seq[coop.rchain.rspace.examples.AddressBookExample.Entry])]]] = Right(None)

scala> val pres7 = space.produce(Channel("colleagues"), dan, persist = false)
pres7: cats.Id[Either[Nothing,Option[(coop.rchain.rspace.examples.AddressBookExample.Printer, scala.collection.immutable.Seq[coop.rchain.rspace.examples.AddressBookExample.Entry])]]] = Right(None)

scala> val pres8 = space.produce(Channel("friends"), erin, persist = false)
pres8: cats.Id[Either[Nothing,Option[(coop.rchain.rspace.examples.AddressBookExample.Printer, scala.collection.immutable.Seq[coop.rchain.rspace.examples.AddressBookExample.Entry])]]] = Right(Some((<function1>,List(Entry(Name(Erin,Rush),Address(23 Market St.,Peony,Idaho,224422),erush@lasttraintogoa.net,333-555-1212), Entry(Name(Dan,Walters),Address(40 Shady Lane,Crystal Lake,Idaho,223322),deejwalters@sdf.lonestar.org,444-555-1212)))))

scala> runK(pres7)
```

Note that we can provide different patterns to match with the data on each channel.
```scala
scala> val cres7 = space.consume(List(Channel("friends"), Channel("colleagues")), List(StateMatch("Idaho"), CityMatch("Crystal Lake")), new Printer, persist = false)
cres7: cats.Id[Either[Nothing,Option[(coop.rchain.rspace.examples.AddressBookExample.Printer, scala.collection.immutable.Seq[coop.rchain.rspace.examples.AddressBookExample.Entry])]]] = Right(None)

scala> val pres9 = space.produce(Channel("colleagues"), dan, persist = false)
pres9: cats.Id[Either[Nothing,Option[(coop.rchain.rspace.examples.AddressBookExample.Printer, scala.collection.immutable.Seq[coop.rchain.rspace.examples.AddressBookExample.Entry])]]] = Right(None)

scala> val pres10 = space.produce(Channel("friends"), erin, persist = false)
pres10: cats.Id[Either[Nothing,Option[(coop.rchain.rspace.examples.AddressBookExample.Printer, scala.collection.immutable.Seq[coop.rchain.rspace.examples.AddressBookExample.Entry])]]] = Right(Some((<function1>,List(Entry(Name(Erin,Rush),Address(23 Market St.,Peony,Idaho,224422),erush@lasttraintogoa.net,333-555-1212), Entry(Name(Dan,Walters),Address(40 Shady Lane,Crystal Lake,Idaho,223322),deejwalters@sdf.lonestar.org,444-555-1212)))))

scala> runK(pres10)

=== ENTRY ===
name:    Rush, Erin
address: 23 Market St., Peony, Idaho 224422
email:   erush@lasttraintogoa.net
phone:   333-555-1212

=== ENTRY ===
name:    Walters, Dan
address: 40 Shady Lane, Crystal Lake, Idaho 223322
email:   deejwalters@sdf.lonestar.org
phone:   444-555-1212
```

###  Making Things Stick

So far we've seen that every matching piece of data or continuation returned to us has also been removed from the store.  We could obviously put it back into the store ourselves, but we can also use `produce` and `consume`'s persist parameter to put something in the store and "make it stick".

Let's try to persist a continuation, but first let's put our Crystal Lake-dwelling friends back in the store.
```scala
scala> val pres11 = space.produce(Channel("friends"), alice, persist = false)
pres11: cats.Id[Either[Nothing,Option[(coop.rchain.rspace.examples.AddressBookExample.Printer, scala.collection.immutable.Seq[coop.rchain.rspace.examples.AddressBookExample.Entry])]]] = Right(None)

scala> val pres12 = space.produce(Channel("friends"), bob, persist = false)
pres12: cats.Id[Either[Nothing,Option[(coop.rchain.rspace.examples.AddressBookExample.Printer, scala.collection.immutable.Seq[coop.rchain.rspace.examples.AddressBookExample.Entry])]]] = Right(None)
```

Now let's try to do a `consume` with the persist flag set to `true`.
```scala
scala> val cres8 = space.consume(List(Channel("friends")), List(CityMatch(city = "Crystal Lake")), new Printer, persist = true)
cres8: cats.Id[Either[Nothing,Option[(coop.rchain.rspace.examples.AddressBookExample.Printer, scala.collection.immutable.Seq[coop.rchain.rspace.examples.AddressBookExample.Entry])]]] = Right(Some((<function1>,List(Entry(Name(Alice,Lincoln),Address(777 Ford St.,Crystal Lake,Idaho,223322),alicel@ringworld.net,777-555-1212)))))
```

Look, data!

Let's run with it.
```scala
scala> runK(cres8)

=== ENTRY ===
name:    Lincoln, Alice
address: 777 Ford St., Crystal Lake, Idaho 223322
email:   alicel@ringworld.net
phone:   777-555-1212
```

As a side note, we can observe the fact there is no particular order to which we retrieve matching data (or continuations).  If multiple matches exist, one is non-deterministically chosen and returned to the caller.

So did our `consume` stick?

```scala
scala> println(space.store.toMap)
Map(List(Channel(friends)) -> Row(List(Datum(Entry(Name(Bob,Lahblah),Address(1000 Main St,Crystal Lake,Idaho,223322),blablah@tenex.net,698-555-1212),false,Produce(channels: Blake2b256Hash(bytes: ByteVector(32 bytes, 0xeac5571fbf9fc71f19d65e14314b32ad06538a364c2ff101edfa99a969fac35b)), hash: Blake2b256Hash(bytes: ByteVector(32 bytes, 0x7b39ca265b2cfdd27fc464b71470c3259a2ddd5f2e37e139e9d1c29ced3a6a31))))),List()))
```

It did not!  That's strange...

This quirk of `rspace` is to address the circumstance where matches already exist for a particular continuation that we are trying to persist.  If we were able to persist the continuation without retrieving the existing matches, those matches might end up "lost" in the store.  Instead, we should keep re-attempting to do a persistent consume until all existing matches have been "drained" from the store.  When we receive a `None`, we know that all existing matches have been returned from the store and the continuation has been persisted.

```scala
scala> val cres9 = space.consume(List(Channel("friends")), List(CityMatch(city = "Crystal Lake")), new Printer, persist = true)
cres9: cats.Id[Either[Nothing,Option[(coop.rchain.rspace.examples.AddressBookExample.Printer, scala.collection.immutable.Seq[coop.rchain.rspace.examples.AddressBookExample.Entry])]]] = Right(Some((<function1>,List(Entry(Name(Bob,Lahblah),Address(1000 Main St,Crystal Lake,Idaho,223322),blablah@tenex.net,698-555-1212)))))

scala> runK(cres9)

=== ENTRY ===
name:    Lahblah, Bob
address: 1000 Main St, Crystal Lake, Idaho 223322
email:   blablah@tenex.net
phone:   698-555-1212

scala> val cres10 = space.consume(List(Channel("friends")), List(CityMatch(city = "Crystal Lake")), new Printer, persist = true)
cres10: cats.Id[Either[Nothing,Option[(coop.rchain.rspace.examples.AddressBookExample.Printer, scala.collection.immutable.Seq[coop.rchain.rspace.examples.AddressBookExample.Entry])]]] = Right(None)

scala> println(space.store.toMap)
Map(List(Channel(friends)) -> Row(List(),List(WaitingContinuation(List(CityMatch(Crystal Lake)),<function1>,true,Consume(channels: Blake2b256Hash(bytes: ByteVector(32 bytes, 0xeac5571fbf9fc71f19d65e14314b32ad06538a364c2ff101edfa99a969fac35b)), hash: Blake2b256Hash(bytes: ByteVector(32 bytes, 0xb04ae03458e620b9c3d06148d5e651fa91c2bb0575ea1f6e5e91ebbf197b6695)))))))
```

The same rule applies for doing a persistent `produce` - if any matching continuations exist in the store, they must be drained before the data will be persisted.

For example,
```scala
scala> val cres11 = space.consume(List(Channel("friends")), List(CityMatch(city = "Peony")), new Printer, persist = false)
cres11: cats.Id[Either[Nothing,Option[(coop.rchain.rspace.examples.AddressBookExample.Printer, scala.collection.immutable.Seq[coop.rchain.rspace.examples.AddressBookExample.Entry])]]] = Right(None)

scala> val pres13 = space.produce(Channel("friends"), erin, persist = true)
pres13: cats.Id[Either[Nothing,Option[(coop.rchain.rspace.examples.AddressBookExample.Printer, scala.collection.immutable.Seq[coop.rchain.rspace.examples.AddressBookExample.Entry])]]] = Right(Some((<function1>,List(Entry(Name(Erin,Rush),Address(23 Market St.,Peony,Idaho,224422),erush@lasttraintogoa.net,333-555-1212)))))

scala> runK(pres13)

=== ENTRY ===
name:    Rush, Erin
address: 23 Market St., Peony, Idaho 224422
email:   erush@lasttraintogoa.net
phone:   333-555-1212

scala> println(space.store.toMap)
Map(List(Channel(friends)) -> Row(List(),List(WaitingContinuation(List(CityMatch(Crystal Lake)),<function1>,true,Consume(channels: Blake2b256Hash(bytes: ByteVector(32 bytes, 0xeac5571fbf9fc71f19d65e14314b32ad06538a364c2ff101edfa99a969fac35b)), hash: Blake2b256Hash(bytes: ByteVector(32 bytes, 0xb04ae03458e620b9c3d06148d5e651fa91c2bb0575ea1f6e5e91ebbf197b6695)))))))

scala> val pres14 = space.produce(Channel("friends"), erin, persist = true)
pres14: cats.Id[Either[Nothing,Option[(coop.rchain.rspace.examples.AddressBookExample.Printer, scala.collection.immutable.Seq[coop.rchain.rspace.examples.AddressBookExample.Entry])]]] = Right(None)

scala> println(space.store.toMap)
Map(List(Channel(friends)) -> Row(List(Datum(Entry(Name(Erin,Rush),Address(23 Market St.,Peony,Idaho,224422),erush@lasttraintogoa.net,333-555-1212),true,Produce(channels: Blake2b256Hash(bytes: ByteVector(32 bytes, 0xeac5571fbf9fc71f19d65e14314b32ad06538a364c2ff101edfa99a969fac35b)), hash: Blake2b256Hash(bytes: ByteVector(32 bytes, 0x7d664473c31bd1b7a7bed21ec1794a509f9ea8d7eaa206657da7c9cf52fd51f6))))),List(WaitingContinuation(List(CityMatch(Crystal Lake)),<function1>,true,Consume(channels: Blake2b256Hash(bytes: ByteVector(32 bytes, 0xeac5571fbf9fc71f19d65e14314b32ad06538a364c2ff101edfa99a969fac35b)), hash: Blake2b256Hash(bytes: ByteVector(32 bytes, 0xb04ae03458e620b9c3d06148d5e651fa91c2bb0575ea1f6e5e91ebbf197b6695)))))))
```

### History & rollback

It is possible to save the current state of RSpace in the form of a `Checkpoint`. A `Checkpoint` value contains the root hash of a Merkle Patricia Trie built from the contents of RSpace.
```scala
val checkpoint = space.createCheckpoint()
val checkpointHash = checkpoint.root
```

To rollback the state of the RSpace to a given `Checkpoint` one simply calls the `reset` method with the hash of the root of the `Checkpoint` provided as parameter.
```scala
space.reset(checkpointHash)
```

Let's see how this works in practice. We'll start by creating a new, untouched RSpace followed by a consume operation which should put data and a continuation at given channel.
```scala
scala> val rollbackExampleStorePath: Path = Files.createTempDirectory("rspace-address-book-example-")
rollbackExampleStorePath: java.nio.file.Path = /var/folders/rk/kf3ndvw96bnd18fcrtf1vjfh0000gn/T/rspace-address-book-example-7312415577584704503

scala> val rollbackExampleContext: Context[Channel, Pattern, Entry, Printer] = Context.create[Channel, Pattern, Entry, Printer](rollbackExampleStorePath,  1024L * 1024L * 100L)
rollbackExampleContext: coop.rchain.rspace.Context[coop.rchain.rspace.examples.AddressBookExample.Channel,coop.rchain.rspace.examples.AddressBookExample.Pattern,coop.rchain.rspace.examples.AddressBookExample.Entry,coop.rchain.rspace.examples.AddressBookExample.Printer] = coop.rchain.rspace.LMDBContext@1deea8b

scala> val rollbackExampleSpace = RSpace.create[Id, Channel, Pattern, Nothing, Entry, Entry, Printer](rollbackExampleContext, coop.rchain.rspace.history.Branch.MASTER)
rollbackExampleSpace: cats.Id[coop.rchain.rspace.ISpace[cats.Id,coop.rchain.rspace.examples.AddressBookExample.Channel,coop.rchain.rspace.examples.AddressBookExample.Pattern,Nothing,coop.rchain.rspace.examples.AddressBookExample.Entry,coop.rchain.rspace.examples.AddressBookExample.Entry,coop.rchain.rspace.examples.AddressBookExample.Printer]] = coop.rchain.rspace.spaces.CoarseGrainedRSpace@5646a9e4

scala> val cres =
     |   rollbackExampleSpace.consume(List(Channel("friends")),
     |                 List(CityMatch(city = "Crystal Lake")),
     |                 new Printer,
     |                 persist = false)
cres: cats.Id[Either[Nothing,Option[(coop.rchain.rspace.examples.AddressBookExample.Printer, scala.collection.immutable.Seq[coop.rchain.rspace.examples.AddressBookExample.Entry])]]] = Right(None)

scala> cres.right.get.isEmpty
res19: Boolean = true
```

We can now create a checkpoint and store it's root.
```scala
scala> val checkpointHash = rollbackExampleSpace.createCheckpoint.root
checkpointHash: coop.rchain.rspace.Blake2b256Hash = Blake2b256Hash(bytes: ByteVector(32 bytes, 0xc9304533ab01f488ffe0692cc42f3b2cb7f231323efe32ee2e3802cd42086719))
```

The first `produceAlice` operation should be able to find data stored by the consume.
```scala
scala> def produceAlice(): Either[Nothing, Option[(Printer, Seq[Entry])]] = rollbackExampleSpace.produce(Channel("friends"), alice, persist = false)
produceAlice: ()Either[Nothing,Option[(coop.rchain.rspace.examples.AddressBookExample.Printer, Seq[coop.rchain.rspace.examples.AddressBookExample.Entry])]]

scala> produceAlice.toOption.flatten.isDefined
res20: Boolean = true
```

Running the same operation again shouldn't return anything, as data hasn't been persisted.
```scala
scala> produceAlice.right.get.isEmpty
res21: Boolean = true
```
Every following repetition of the operation above should yield an empty result.
```scala
scala> produceAlice.right.get.isEmpty
res22: Boolean = true
```

After re-setting the RSpace to the state from the saved checkpoint the first produce operation should again return an non-empty result.
```scala
scala> rollbackExampleSpace.reset(checkpointHash)
res23: cats.Id[Unit] = ()

scala> produceAlice.toOption.flatten.isDefined
res24: Boolean = true
```
And again, every following operation should yield an empty result
Every following repetition of the operation above should yield an empty result.
```scala
scala> produceAlice.right.get.isEmpty
res25: Boolean = true
```

### Finishing Up

When we are finished using the spaces, we close them.
```scala
scala> space.close()
res26: cats.Id[Unit] = ()

scala> rollbackExampleSpace.close()
res27: cats.Id[Unit] = ()
```
