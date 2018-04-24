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

  def encode(a: A): Array[Byte]

  def decode(bytes: Array[Byte]): Either[Throwable, A]
}
```

Let's try defining an instance of `Serialize` for `Channel` using Java serialization.

First we will need to import some more stuff.
```scala
scala> import java.io.{ByteArrayInputStream, ByteArrayOutputStream, ObjectInputStream, ObjectOutputStream}
import java.io.{ByteArrayInputStream, ByteArrayOutputStream, ObjectInputStream, ObjectOutputStream}
```

Now we define an instance of `Serialize`.
```scala
scala> implicit object serializeChannel extends Serialize[Channel] {
     | 
     |   def encode(channel: Channel): Array[Byte] = {
     |     val baos = new ByteArrayOutputStream()
     |     try {
     |       val oos = new ObjectOutputStream(baos)
     |       try { oos.writeObject(channel) } finally { oos.close() }
     |       baos.toByteArray
     |     } finally {
     |       baos.close()
     |     }
     |   }
     | 
     |   def decode(bytes: Array[Byte]): Either[Throwable, Channel] = {
     |     try {
     |       val bais = new ByteArrayInputStream(bytes)
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
serializeEntry: coop.rchain.rspace.Serialize[coop.rchain.rspace.examples.AddressBookExample.Entry] = coop.rchain.rspace.examples.package$$anon$1@70319098

scala> implicit val serializePattern: Serialize[Pattern] = makeSerializeFromSerializable[Pattern]
serializePattern: coop.rchain.rspace.Serialize[coop.rchain.rspace.examples.AddressBookExample.Pattern] = coop.rchain.rspace.examples.package$$anon$1@494a3f64

scala> implicit val serializePrinter: Serialize[Printer] = makeSerializeFromSerializable[Printer]
serializePrinter: coop.rchain.rspace.Serialize[coop.rchain.rspace.examples.AddressBookExample.Printer] = coop.rchain.rspace.examples.package$$anon$1@59218a52
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
  * @tparam A A type representing data
  */
trait Match[P, A] {

  def get(p: P, a: A): Option[A]
}
```

Let's try defining an instance of `Match` for `Pattern` and `Entry`.
```scala
scala> implicit object matchPatternEntry extends Match[Pattern, Entry] {
     |   def get(p: Pattern, a: Entry): Option[Entry] =
     |     p match {
     |       case NameMatch(last) if a.name.last == last        => Some(a)
     |       case CityMatch(city) if a.address.city == city     => Some(a)
     |       case StateMatch(state) if a.address.state == state => Some(a)
     |       case _                                             => None
     |     }
     | }
defined object matchPatternEntry
```

Let's see this instance in action.
```scala
scala> val mat = matchPatternEntry.get(NameMatch("Lincoln"), alice)
mat: Option[coop.rchain.rspace.examples.AddressBookExample.Entry] = Some(Entry(Name(Alice,Lincoln),Address(777 Ford St.,Crystal Lake,Idaho,223322),alicel@ringworld.net,777-555-1212))

scala> val noMat = matchPatternEntry.get(NameMatch("Lincoln"), bob)
noMat: Option[coop.rchain.rspace.examples.AddressBookExample.Entry] = None
```

### Instantiating the Store

First we define a temporary place to put the store's files.
```scala
scala> import java.nio.file.{Files, Path}
import java.nio.file.{Files, Path}

scala> val storePath: Path = Files.createTempDirectory("rspace-address-book-example-")
storePath: java.nio.file.Path = /var/folders/95/k150s30j5yj6tt31f1v49hsr0000gn/T/rspace-address-book-example-2121142630754763137
```

Next we create an instance of `LMDBStore` using `storePath`.  We will create our store with a maximum map size of 100MB.
```scala
scala> val store: LMDBStore[Channel, Pattern, Entry, Printer] = LMDBStore.create[Channel, Pattern, Entry, Printer](storePath, 1024L * 1024L * 100L)
store: coop.rchain.rspace.LMDBStore[coop.rchain.rspace.examples.AddressBookExample.Channel,coop.rchain.rspace.examples.AddressBookExample.Pattern,coop.rchain.rspace.examples.AddressBookExample.Entry,coop.rchain.rspace.examples.AddressBookExample.Printer] = coop.rchain.rspace.LMDBStore@48137d28
```

### Producing and Consuming

First we will install a "query" in the store using `consume`.
```scala
scala> val cres1 = consume(store, List(Channel("friends")), List(CityMatch(city = "Crystal Lake")), new Printer, persist = false)
cres1: Option[(coop.rchain.rspace.examples.AddressBookExample.Printer, scala.collection.immutable.Seq[coop.rchain.rspace.examples.AddressBookExample.Entry])] = None
```

Here we are installing a continuation in the store at the "friends" `Channel`.  This continuation will be returned to us when a piece of matching data is introduced to the store at that channel.

Now let's try introducing a piece of data to the store using `produce`.
```scala
scala> val pres1 = produce(store, Channel("friends"), alice, persist = false)
pres1: Option[(coop.rchain.rspace.examples.AddressBookExample.Printer, scala.collection.immutable.Seq[coop.rchain.rspace.examples.AddressBookExample.Entry])] = Some((<function1>,List(Entry(Name(Alice,Lincoln),Address(777 Ford St.,Crystal Lake,Idaho,223322),alicel@ringworld.net,777-555-1212))))
```

If we look closely at the result, we can see that we have received back an `Option` containing a `<function1>` along with the data we passed to `produce`.  This is because this data has satisfied the match with the `Pattern` we provided in our call to `consume`.

Let's run the continuation using a function from the `extended` package.
```scala
scala> import coop.rchain.rspace.extended._
import coop.rchain.rspace.extended._

scala> runK(pres1)

=== ENTRY ===
name:    Lincoln, Alice
address: 777 Ford St., Crystal Lake, Idaho 223322
email:   alicel@ringworld.net
phone:   777-555-1212
```

When we inspect the contents of the store, we notice that the store is empty.  The continuation has been removed.  The data was also not stored because a matching continuation was found.
```scala
scala> println(store.toMap)
Map()
```

Let's reinstall the same continuation.
```scala
scala> val cres2 = consume(store, List(Channel("friends")), List(CityMatch(city = "Crystal Lake")), new Printer, persist = false)
cres2: Option[(coop.rchain.rspace.examples.AddressBookExample.Printer, scala.collection.immutable.Seq[coop.rchain.rspace.examples.AddressBookExample.Entry])] = None
```

Now let's try introducing another piece of data to the store.
```scala
scala> val pres2 = produce(store, Channel("friends"), bob, persist = false)
pres2: Option[(coop.rchain.rspace.examples.AddressBookExample.Printer, scala.collection.immutable.Seq[coop.rchain.rspace.examples.AddressBookExample.Entry])] = Some((<function1>,List(Entry(Name(Bob,Lahblah),Address(1000 Main St,Crystal Lake,Idaho,223322),blablah@tenex.net,698-555-1212))))
```

Let's also run the continuation and inspect the store.
```scala
scala> runK(pres2)

=== ENTRY ===
name:    Lahblah, Bob
address: 1000 Main St, Crystal Lake, Idaho 223322
email:   blablah@tenex.net
phone:   698-555-1212

scala> println(store.toMap)
Map()
```

Now let's reinstall the the continuation and introduce another piece of data to the store.
```scala
scala> val cres3 = consume(store, List(Channel("friends")), List(CityMatch(city = "Crystal Lake")), new Printer, persist = false)
cres3: Option[(coop.rchain.rspace.examples.AddressBookExample.Printer, scala.collection.immutable.Seq[coop.rchain.rspace.examples.AddressBookExample.Entry])] = None

scala> val pres3 = produce(store, Channel("friends"), carol, persist = false)
pres3: Option[(coop.rchain.rspace.examples.AddressBookExample.Printer, scala.collection.immutable.Seq[coop.rchain.rspace.examples.AddressBookExample.Entry])] = None
```

This time we receive a `None` back from `produce`, indicating that no match has been made.

Let's inspect the contents of the store again.
```scala
scala> println(store.toMap)
Map(List(Channel(friends)) -> Row(List(Datum(Entry(Name(Carol,Lahblah),Address(22 Goldwater Way,Herbert,Nevada,334433),carol@blablah.org,232-555-1212),false)),List(WaitingContinuation(List(CityMatch(Crystal Lake)),<function1>,false))))
```

Here we can see that that at `Channel("friends")`, we have now stored Carol's `Entry` record along with our continuations and its patterns.

Let's `consume` again, but this time, let's use a different pattern - one that will match Carol's `Entry` - returning it to us.
```scala
scala> val cres4 = consume(store, List(Channel("friends")), List(NameMatch(last = "Lahblah")), new Printer, persist = false)
cres4: Option[(coop.rchain.rspace.examples.AddressBookExample.Printer, scala.collection.immutable.Seq[coop.rchain.rspace.examples.AddressBookExample.Entry])] = Some((<function1>,List(Entry(Name(Carol,Lahblah),Address(22 Goldwater Way,Herbert,Nevada,334433),carol@blablah.org,232-555-1212))))

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
scala> println(store.toMap)
Map(List(Channel(friends)) -> Row(List(),List(WaitingContinuation(List(CityMatch(Crystal Lake)),<function1>,false))))
```

Let's produce one more time to let this sink in.
```scala
scala> val pres4 = produce(store, Channel("friends"), alice, persist = false)
pres4: Option[(coop.rchain.rspace.examples.AddressBookExample.Printer, scala.collection.immutable.Seq[coop.rchain.rspace.examples.AddressBookExample.Entry])] = Some((<function1>,List(Entry(Name(Alice,Lincoln),Address(777 Ford St.,Crystal Lake,Idaho,223322),alicel@ringworld.net,777-555-1212))))

scala> runK(pres4)

=== ENTRY ===
name:    Lincoln, Alice
address: 777 Ford St., Crystal Lake, Idaho 223322
email:   alicel@ringworld.net
phone:   777-555-1212

scala> println(store.toMap)
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
scala> val pres5 = produce(store, Channel("colleagues"), dan, persist = false)
pres5: Option[(coop.rchain.rspace.examples.AddressBookExample.Printer, scala.collection.immutable.Seq[coop.rchain.rspace.examples.AddressBookExample.Entry])] = None

scala> val pres6 = produce(store, Channel("friends"), erin, persist = false)
pres6: Option[(coop.rchain.rspace.examples.AddressBookExample.Printer, scala.collection.immutable.Seq[coop.rchain.rspace.examples.AddressBookExample.Entry])] = None
```

Now let's `consume` on multiple channels, searching for our friends and colleagues who live in Idaho.
```scala
scala> val cres5 = consume(store, List(Channel("friends"), Channel("colleagues")), List(StateMatch("Idaho"), StateMatch("Idaho")), new Printer, persist = false)
cres5: Option[(coop.rchain.rspace.examples.AddressBookExample.Printer, scala.collection.immutable.Seq[coop.rchain.rspace.examples.AddressBookExample.Entry])] = Some((<function1>,List(Entry(Name(Erin,Rush),Address(23 Market St.,Peony,Idaho,224422),erush@lasttraintogoa.net,333-555-1212), Entry(Name(Dan,Walters),Address(40 Shady Lane,Crystal Lake,Idaho,223322),deejwalters@sdf.lonestar.org,444-555-1212))))

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
scala> val cres6 = consume(store, List(Channel("friends"), Channel("colleagues")), List(StateMatch("Idaho"), StateMatch("Idaho")), new Printer, persist = false)
cres6: Option[(coop.rchain.rspace.examples.AddressBookExample.Printer, scala.collection.immutable.Seq[coop.rchain.rspace.examples.AddressBookExample.Entry])] = None

scala> val pres7 = produce(store, Channel("colleagues"), dan, persist = false)
pres7: Option[(coop.rchain.rspace.examples.AddressBookExample.Printer, scala.collection.immutable.Seq[coop.rchain.rspace.examples.AddressBookExample.Entry])] = None

scala> val pres8 = produce(store, Channel("friends"), erin, persist = false)
pres8: Option[(coop.rchain.rspace.examples.AddressBookExample.Printer, scala.collection.immutable.Seq[coop.rchain.rspace.examples.AddressBookExample.Entry])] = Some((<function1>,List(Entry(Name(Erin,Rush),Address(23 Market St.,Peony,Idaho,224422),erush@lasttraintogoa.net,333-555-1212), Entry(Name(Dan,Walters),Address(40 Shady Lane,Crystal Lake,Idaho,223322),deejwalters@sdf.lonestar.org,444-555-1212))))

scala> runK(pres7)
```

Note that we can provide different patterns to match with the data on each channel.
```scala
scala> val cres7 = consume(store, List(Channel("friends"), Channel("colleagues")), List(StateMatch("Idaho"), CityMatch("Crystal Lake")), new Printer, persist = false)
cres7: Option[(coop.rchain.rspace.examples.AddressBookExample.Printer, scala.collection.immutable.Seq[coop.rchain.rspace.examples.AddressBookExample.Entry])] = None

scala> val pres9 = produce(store, Channel("colleagues"), dan, persist = false)
pres9: Option[(coop.rchain.rspace.examples.AddressBookExample.Printer, scala.collection.immutable.Seq[coop.rchain.rspace.examples.AddressBookExample.Entry])] = None

scala> val pres10 = produce(store, Channel("friends"), erin, persist = false)
pres10: Option[(coop.rchain.rspace.examples.AddressBookExample.Printer, scala.collection.immutable.Seq[coop.rchain.rspace.examples.AddressBookExample.Entry])] = Some((<function1>,List(Entry(Name(Erin,Rush),Address(23 Market St.,Peony,Idaho,224422),erush@lasttraintogoa.net,333-555-1212), Entry(Name(Dan,Walters),Address(40 Shady Lane,Crystal Lake,Idaho,223322),deejwalters@sdf.lonestar.org,444-555-1212))))

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
scala> val pres11 = produce(store, Channel("friends"), alice, persist = false)
pres11: Option[(coop.rchain.rspace.examples.AddressBookExample.Printer, scala.collection.immutable.Seq[coop.rchain.rspace.examples.AddressBookExample.Entry])] = None

scala> val pres12 = produce(store, Channel("friends"), bob, persist = false)
pres12: Option[(coop.rchain.rspace.examples.AddressBookExample.Printer, scala.collection.immutable.Seq[coop.rchain.rspace.examples.AddressBookExample.Entry])] = None
```

Now let's try to do a `consume` with the persist flag set to `true`.
```scala
scala> val cres8 = consume(store, List(Channel("friends")), List(CityMatch(city = "Crystal Lake")), new Printer, persist = true)
cres8: Option[(coop.rchain.rspace.examples.AddressBookExample.Printer, scala.collection.immutable.Seq[coop.rchain.rspace.examples.AddressBookExample.Entry])] = Some((<function1>,List(Entry(Name(Alice,Lincoln),Address(777 Ford St.,Crystal Lake,Idaho,223322),alicel@ringworld.net,777-555-1212))))
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
scala> println(store.toMap)
Map(List(Channel(friends)) -> Row(List(Datum(Entry(Name(Bob,Lahblah),Address(1000 Main St,Crystal Lake,Idaho,223322),blablah@tenex.net,698-555-1212),false)),List()))
```

It did not!  That's strange...

This quirk of `rspace` is to address the circumstance where matches already exist for a particular continuation that we are trying to persist.  If we were able to persist the continuation without retrieving the existing matches, those matches might end up "lost" in the store.  Instead, we should keep re-attempting to do a persistent consume until all existing matches have been "drained" from the store.  When we receive a `None`, we know that all existing matches have been returned from the store and the continuation has been persisted.

```scala
scala> val cres9 = consume(store, List(Channel("friends")), List(CityMatch(city = "Crystal Lake")), new Printer, persist = true)
cres9: Option[(coop.rchain.rspace.examples.AddressBookExample.Printer, scala.collection.immutable.Seq[coop.rchain.rspace.examples.AddressBookExample.Entry])] = Some((<function1>,List(Entry(Name(Bob,Lahblah),Address(1000 Main St,Crystal Lake,Idaho,223322),blablah@tenex.net,698-555-1212))))

scala> runK(cres9)

=== ENTRY ===
name:    Lahblah, Bob
address: 1000 Main St, Crystal Lake, Idaho 223322
email:   blablah@tenex.net
phone:   698-555-1212

scala> val cres10 = consume(store, List(Channel("friends")), List(CityMatch(city = "Crystal Lake")), new Printer, persist = true)
cres10: Option[(coop.rchain.rspace.examples.AddressBookExample.Printer, scala.collection.immutable.Seq[coop.rchain.rspace.examples.AddressBookExample.Entry])] = None

scala> println(store.toMap)
Map(List(Channel(friends)) -> Row(List(),List(WaitingContinuation(List(CityMatch(Crystal Lake)),<function1>,true))))
```

The same rule applies for doing a persistent `produce` - if any matching continuations exist in the store, they must be drained before the data will be persisted.

For example,
```scala
scala> val cres11 = consume(store, List(Channel("friends")), List(CityMatch(city = "Peony")), new Printer, persist = false)
cres11: Option[(coop.rchain.rspace.examples.AddressBookExample.Printer, scala.collection.immutable.Seq[coop.rchain.rspace.examples.AddressBookExample.Entry])] = None

scala> val pres13 = produce(store, Channel("friends"), erin, persist = true)
pres13: Option[(coop.rchain.rspace.examples.AddressBookExample.Printer, scala.collection.immutable.Seq[coop.rchain.rspace.examples.AddressBookExample.Entry])] = Some((<function1>,List(Entry(Name(Erin,Rush),Address(23 Market St.,Peony,Idaho,224422),erush@lasttraintogoa.net,333-555-1212))))

scala> runK(pres13)

=== ENTRY ===
name:    Rush, Erin
address: 23 Market St., Peony, Idaho 224422
email:   erush@lasttraintogoa.net
phone:   333-555-1212

scala> println(store.toMap)
Map(List(Channel(friends)) -> Row(List(),List(WaitingContinuation(List(CityMatch(Crystal Lake)),<function1>,true))))

scala> val pres14 = produce(store, Channel("friends"), erin, persist = true)
pres14: Option[(coop.rchain.rspace.examples.AddressBookExample.Printer, scala.collection.immutable.Seq[coop.rchain.rspace.examples.AddressBookExample.Entry])] = None

scala> println(store.toMap)
Map(List(Channel(friends)) -> Row(List(Datum(Entry(Name(Erin,Rush),Address(23 Market St.,Peony,Idaho,224422),erush@lasttraintogoa.net,333-555-1212),true)),List(WaitingContinuation(List(CityMatch(Crystal Lake)),<function1>,true))))
```

### Finishing Up

When we are finished using the store, we close it.
```scala
scala> store.close()
```
