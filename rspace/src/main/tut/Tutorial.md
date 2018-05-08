# An rspace tutorial

### Getting Started


To start using `rspace`, we first import the library into our project.
```tut
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

```tut
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
```tut
import java.io.{ByteArrayInputStream, ByteArrayOutputStream, ObjectInputStream, ObjectOutputStream}
```

Now we define an instance of `Serialize`.
```tut
implicit object serializeChannel extends Serialize[Channel] {

  def encode(channel: Channel): Array[Byte] = {
    val baos = new ByteArrayOutputStream()
    try {
      val oos = new ObjectOutputStream(baos)
      try { oos.writeObject(channel) } finally { oos.close() }
      baos.toByteArray
    } finally {
      baos.close()
    }
  }

  def decode(bytes: Array[Byte]): Either[Throwable, Channel] = {
    try {
      val bais = new ByteArrayInputStream(bytes)
      try {
        val ois = new ObjectInputStream(bais)
        try { Right(ois.readObject.asInstanceOf[Channel]) } finally { ois.close() }
      } finally {
        bais.close()
      }
    } catch {
      case ex: Throwable => Left(ex)
    }
  }
}
```

Let's try using it.
```tut
val chan = new Channel("friends")

val roundTrippedChan = serializeChannel.decode(serializeChannel.encode(chan))

```

Looking at the instance we above we could actually write a function to generate these instances for any type `T` that's a subtype of `scala.Serializable`.

In fact, we have provided this convenience function in the `coop.rchain.rspace.examples` package.

```tut
import coop.rchain.rspace.examples._
```

Let's go ahead use that to make instances for our other types.
```tut
implicit val serializeEntry: Serialize[Entry] = makeSerializeFromSerializable[Entry]
implicit val serializePattern: Serialize[Pattern] = makeSerializeFromSerializable[Pattern]
implicit val serializePrinter: Serialize[Printer] = makeSerializeFromSerializable[Printer]
```

Now we will define some example `Entry`s.
```tut
val alice = Entry(name = Name("Alice", "Lincoln"),
                  address = Address("777 Ford St.", "Crystal Lake", "Idaho", "223322"),
                  email = "alicel@ringworld.net",
                  phone = "777-555-1212")

val bob = Entry(name = Name("Bob", "Lahblah"),
                address = Address("1000 Main St", "Crystal Lake", "Idaho", "223322"),
                email = "blablah@tenex.net",
                phone = "698-555-1212")

val carol = Entry(name = Name("Carol", "Lahblah"),
                  address = Address("22 Goldwater Way", "Herbert", "Nevada", "334433"),
                  email = "carol@blablah.org",
                  phone = "232-555-1212")
```

Let's test out one of our other new `Serialize` instances with one of these values:
```tut
val roundTrippedAlice = serializeEntry.decode(serializeEntry.encode(alice))
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
```tut
implicit object matchPatternEntry extends Match[Pattern, Entry] {
  def get(p: Pattern, a: Entry): Option[Entry] =
    p match {
      case NameMatch(last) if a.name.last == last        => Some(a)
      case CityMatch(city) if a.address.city == city     => Some(a)
      case StateMatch(state) if a.address.state == state => Some(a)
      case _                                             => None
    }
}

```

Let's see this instance in action.
```tut
val mat = matchPatternEntry.get(NameMatch("Lincoln"), alice)
val noMat = matchPatternEntry.get(NameMatch("Lincoln"), bob)
```

### Instantiating the Store

First we define a temporary place to put the store's files.
```tut
import java.nio.file.{Files, Path}
val storePath: Path = Files.createTempDirectory("rspace-address-book-example-")
```

Next we create an instance of `LMDBStore` using `storePath`.  We will create our store with a maximum map size of 100MB.
```tut
val store: LMDBStore[Channel, Pattern, Entry, Printer] = LMDBStore.create[Channel, Pattern, Entry, Printer](storePath, 1024L * 1024L * 100L)
```

### Producing and Consuming

First we will install a "query" in the store using `consume`.
```tut
val cres1 = consume(store, List(Channel("friends")), List(CityMatch(city = "Crystal Lake")), new Printer, persist = false)
```

Here we are installing a continuation in the store at the "friends" `Channel`.  This continuation will be returned to us when a piece of matching data is introduced to the store at that channel.

Now let's try introducing a piece of data to the store using `produce`.
```tut
val pres1 = produce(store, Channel("friends"), alice, persist = false)
```

If we look closely at the result, we can see that we have received back an `Option` containing a `<function1>` along with the data we passed to `produce`.  This is because this data has satisfied the match with the `Pattern` we provided in our call to `consume`.

Let's run the continuation using a function from the `extended` package.
```tut
import coop.rchain.rspace.extended._
runK(pres1)
```

When we inspect the contents of the store, we notice that the store is empty.  The continuation has been removed.  The data was also not stored because a matching continuation was found.
```tut
println(store.toMap)
```

Let's reinstall the same continuation.
```tut
val cres2 = consume(store, List(Channel("friends")), List(CityMatch(city = "Crystal Lake")), new Printer, persist = false)
```

Now let's try introducing another piece of data to the store.
```tut
val pres2 = produce(store, Channel("friends"), bob, persist = false)
```

Let's also run the continuation and inspect the store.
```tut
runK(pres2)
println(store.toMap)
```

Now let's reinstall the the continuation and introduce another piece of data to the store.
```tut
val cres3 = consume(store, List(Channel("friends")), List(CityMatch(city = "Crystal Lake")), new Printer, persist = false)
val pres3 = produce(store, Channel("friends"), carol, persist = false)
```

This time we receive a `None` back from `produce`, indicating that no match has been made.

Let's inspect the contents of the store again.
```tut
println(store.toMap)
```

Here we can see that that at `Channel("friends")`, we have now stored Carol's `Entry` record along with our continuations and its patterns.

Let's `consume` again, but this time, let's use a different pattern - one that will match Carol's `Entry` - returning it to us.
```tut
val cres4 = consume(store, List(Channel("friends")), List(NameMatch(last = "Lahblah")), new Printer, persist = false)
runK(cres4)
```

Indeed, our call to `consume` has returned another continuation along with Carol's `Entry`.

In the store, we see that there is still a waiting continuation at `Channel("friends")`
```tut
println(store.toMap)
```

Let's produce one more time to let this sink in.
```tut
val pres4 = produce(store, Channel("friends"), alice, persist = false)
runK(pres4)
println(store.toMap)
```


### Using Multiple Channels

Now we are going to experiment with multiple channels.  Let's first define a few more `Entry` values - one for our colleague Dan and another for our friend Erin.
```tut
val dan = Entry(name = Name("Dan", "Walters"),
                address = Address("40 Shady Lane", "Crystal Lake", "Idaho", "223322"),
                email = "deejwalters@sdf.lonestar.org",
                phone = "444-555-1212")
val erin = Entry(name = Name("Erin", "Rush"),
                 address = Address("23 Market St.", "Peony", "Idaho", "224422"),
                 email = "erush@lasttraintogoa.net",
                 phone = "333-555-1212")
```

Let's introduce Dan and Erin to the store with `produce`.
```tut
val pres5 = produce(store, Channel("colleagues"), dan, persist = false)
val pres6 = produce(store, Channel("friends"), erin, persist = false)
```

Now let's `consume` on multiple channels, searching for our friends and colleagues who live in Idaho.
```tut
val cres5 = consume(store, List(Channel("friends"), Channel("colleagues")), List(StateMatch("Idaho"), StateMatch("Idaho")), new Printer, persist = false)
runK(cres5)
```

Now let's do the same thing in the opposite order.
```tut
val cres6 = consume(store, List(Channel("friends"), Channel("colleagues")), List(StateMatch("Idaho"), StateMatch("Idaho")), new Printer, persist = false)
val pres7 = produce(store, Channel("colleagues"), dan, persist = false)
val pres8 = produce(store, Channel("friends"), erin, persist = false)
runK(pres7)
```

Note that we can provide different patterns to match with the data on each channel.
```tut
val cres7 = consume(store, List(Channel("friends"), Channel("colleagues")), List(StateMatch("Idaho"), CityMatch("Crystal Lake")), new Printer, persist = false)
val pres9 = produce(store, Channel("colleagues"), dan, persist = false)
val pres10 = produce(store, Channel("friends"), erin, persist = false)
runK(pres10)
```

###  Making Things Stick

So far we've seen that every matching piece of data or continuation returned to us has also been removed from the store.  We could obviously put it back into the store ourselves, but we can also use `produce` and `consume`'s persist parameter to put something in the store and "make it stick".

Let's try to persist a continuation, but first let's put our Crystal Lake-dwelling friends back in the store.
```tut
val pres11 = produce(store, Channel("friends"), alice, persist = false)
val pres12 = produce(store, Channel("friends"), bob, persist = false)
```

Now let's try to do a `consume` with the persist flag set to `true`.
```tut
val cres8 = consume(store, List(Channel("friends")), List(CityMatch(city = "Crystal Lake")), new Printer, persist = true)
```

Look, data!

Let's run with it.
```tut
runK(cres8)
```

As a side note, we can observe the fact there is no particular order to which we retrieve matching data (or continuations).  If multiple matches exist, one is non-deterministically chosen and returned to the caller.

So did our `consume` stick?

```tut
println(store.toMap)
```

It did not!  That's strange...

This quirk of `rspace` is to address the circumstance where matches already exist for a particular continuation that we are trying to persist.  If we were able to persist the continuation without retrieving the existing matches, those matches might end up "lost" in the store.  Instead, we should keep re-attempting to do a persistent consume until all existing matches have been "drained" from the store.  When we receive a `None`, we know that all existing matches have been returned from the store and the continuation has been persisted.

```tut
val cres9 = consume(store, List(Channel("friends")), List(CityMatch(city = "Crystal Lake")), new Printer, persist = true)
runK(cres9)
val cres10 = consume(store, List(Channel("friends")), List(CityMatch(city = "Crystal Lake")), new Printer, persist = true)
println(store.toMap)
```

The same rule applies for doing a persistent `produce` - if any matching continuations exist in the store, they must be drained before the data will be persisted.

For example,
```tut
val cres11 = consume(store, List(Channel("friends")), List(CityMatch(city = "Peony")), new Printer, persist = false)
val pres13 = produce(store, Channel("friends"), erin, persist = true)
runK(pres13)
println(store.toMap)
val pres14 = produce(store, Channel("friends"), erin, persist = true)
println(store.toMap)
```

### Finishing Up

When we are finished using the store, we close it.
```tut
store.close()
```
