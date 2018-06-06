# A Rholang tutorial

Rholang is a new programming language designed for use in distributed systems.  Like all newborn things, it is growing and changing rapidly; this document describes the syntax in the latest version of RNode.

Rholang is "process-oriented": all computation is done by means of message passing.  Messages are passed on "channels", which are rather like message queues but behave like sets rather than queues.  Rholang is completely asynchronous, in the sense that while you can read a message from a channel and then do something with it, you can't send a message and then do something once it has been received---at least, not without explicitly waiting for an acknowledgment message from the receiver. Note that throughout this document the words "name" and "channel" are used interchangeably. This is because in the rho-calculus (on which Rholang is based) the term name is used, however because you can send and receive information on names, semantically they are like channels.

## Getting started

There is not an IDE for Rholang. Get started with Rholang by selecting one of the options below.
* __Run Rholang on RNode__ - Write Rholang contracts in an editor of your choice and run them on RNode using either the REPL or EVAL modes. [Get started](https://github.com/rchain/rchain/releases) with the latest version of RNode.
* __Run Rholang on a web interface__ - This [web interface](http://rchain.cloud) was created by a RChain community member.
* __Write Rholang using an IntelliJ plugin__ - This [Rholang IntelliJ plugin](https://github.com/tgrospic/rholang-idea) was created by a RChain community member.

## Contracts and sending data

    1 new HelloWorld in {
    2   contract HelloWorld(return) = {
    3     return!("Hello, World!")
    4   } |
    5   new myChannel in {
    6     HelloWorld!(*myChannel)
    7   }
    8 }

1-2) To create a new, private channel, we use the `new ... in` construction. No other process can send or receive messages over this channel unless we explicitly send this channel to the other process.  This process starts by creating a new name `HelloWorld` and then running a contract on it.  The `contract` production creates a process that spawns a copy of its body whenever it receives a message. 

3) On the return channel we send a process, which is the string `"Hello, World!"`.

6) We send the channel `myChannel` to the contract at `HelloWorld`. The `*` operator "unquotes" a channel to get its underlying process. In Rholang you can only send processes over channels; you cannot send channels over channels directly. Therefore, we use `*` to turn the private channel into a process prior to sending.

## Receiving data

    1 new HelloAgain in {
    2   contract HelloAgain(_) = {
    3     new chan in {
    4       chan!("Hello again, world!") |
    5       for (@text <- chan) { Nil }
    6     }
    7   } | HelloAgain!(Nil)
    8 }


2) Contracts take at least one parameter, but we can throw it away by binding it to the wildcard `_`.

3) We create a new channel `chan`.

4) We send the string process `"Hello again, world!"` over the new channel.

5) We listen on the new channel for a single message.  The `for` operation blocks until there's a message available on the channel `chan`. In Rholang you can only receive names on channels (note that this differs from sending!). The binding on the left side of the `<-` in the `for` is actually a name pattern. In this example the pattern is `@text`, which means the name being received is a quoted process and we want to bind that process to the free variable `text`. The `for` operation is just like a contract except that it only reads one message and then becomes its body instead of forking a copy of its body for each message. In this case we choose to do nothing in the `for` body by simply making it the stopped process `Nil`, however in principle we would want to proceed with some further processing of the `text` contained in `chan`. 

7) We trigger the contract.

## Mutable state

     1 new MakeCell in {
     2   // Makes a single cell in which you can store values
     3   contract MakeCell(@init, get, set) = {
     4     new valueStore in {
     5       valueStore!(init) |
     6       contract get(ack) = {
     7         for(@value <- valueStore) {
     8           valueStore!(value) | ack!(value)
     9         }
    10       } |
    11       contract set(@newValue, ack) = {
    12         for(_ <- valueStore) {
    13           valueStore!(newValue) | ack!(true)
    14         }
    15       }
    16     }
    17   } |
    18   // Cell usage.
    19   new myGet, mySet in {
    20     MakeCell!(123, *myGet, *mySet) |
    21     new ack in {
    22       myGet!(*ack) |
    23       for (@result <- ack) {
    24         //result now contains the value 123
    25         mySet!(456, *ack) |
    26         for (_ <- ack) {
    27           myGet!(*ack) |
    28           for (@result <- ack) {
    29             //result now contains the value 456
    30             Nil
    31           }
    32         }
    33       }
    34     }
    35   }
    36 }

1) We create a new channel MakeCell and then use it on line 3 as the name of an internal contract.  No process other than the code inside this lexical scope can invoke it.

3) The `MakeCell` contract takes three arguments. The first argument is the initial value to be stored in the cell. The second and third arguments are channels over which the cell will receive requests to get and set the value. Note that we want the first argument to be a process and the second and third to be names, but names are always received over channels so we need to make the first argument a pattern beginning with `@` to indicate that the name we receive as the first argument is a quoted process and it is that process which we want to bind to the variable.

4) To store the value, we create a new channel.  This channel will have at most one message on it containing the current value of the cell.  

5) Before this line, there are no messages on the `valueStore` channel.  After we send the initial value, it is the only value on that channel.

6) We set up a contract to listen on the `get` channel.  Each time a message is sent on `get`, the body of the contract will be executed

7) We block until we get one message from the `valueStore` channel.  Because there is at most one message ever waiting on `valueStore`, reading the message behaves much like acquiring a lock.

8) We send the current value on `valueStore` again, allowing other messages to be processed (i.e. releasing the lock), and we send the current value back to the client on the `ack` channel.

11) In concurrently with the `get` contract, we run a contract listening on `set`.

12) We block until there's a message on `valueStore`, then read it.  We throw away the message that we read.

13) We send the new value to store on `valueStore` and signal that the operation is complete.

18-36) The usage code demonstrates creating a cell, assigning the initial value 123, getting that value, setting the value to 456, then getting that value.  

Note the deep layers of callback. Rholang was designed to make concurrent computations natural to express; as a consequence, data dependencies implicit in sequencing in other languages must be made explicit.

## Iteration and matching

In the code below, we show an example of iterating through a list.

     1	new iterate in {
     2	  contract iterate(@list, process, done) = {
     3	    match list {
     4	      [hd, ...tl] => {
     5	        new ack in {
     6	          process!(hd, *ack) |
     7	          for (_ <- ack) { iterate!(tl, *process, *done) }
     8	        }
     9	      }
    10	      _ => done!(Nil)
    11	    }
    12	  } |
    13	  new process, done in {
    14	    iterate!([4,5,6], *process, *done) |
    15	    contract process(@item, ack) = {
    16	      /* handle processing of item */
    17	      ack!(Nil)
    18	    } |
    19	    for (_ <- done) {
    20	      /* done! */
    21	      Nil
    22	    }
    23	  }
    24	}

3) The `match` construction allows destructuring a variable through pattern matching.

4) List patterns support matching the remainder of a list.  If `list` matches the pattern of a head/tail pair then we execute the main body of the loop.

5) We create a channel for the item handler to notify us that it's done with the current item.

6) We invoke the processor on the item and the acknowledgement channel.

7) When we receive acknowledgement, we reinvoke the iterator on the tail.

10) If the list is empty, we signal that the processing is complete.

14) We invoke the iterator.

15-18) This `contract` gets invoked for each item in the list.  On line 17, we tell the iterator that we're done with this item.

19) This `for` contains the code that should be executed when the interation is complete.

## Maps

     1 new MakeCoatCheck in {
     2   contract MakeCoatCheck(ret) = {
     3     new port, table in {
     4       ret!(*port) |
     5       for(@"new", @arg, ack <= port) {
     6         new ticket in {
     7           ack!(*ticket) |
     8           @{*ticket | *table}!(arg)
     9         }
    10       } |
    11       for(@"get", @arg, ack <= port) {
    12         for (@value <- @{arg | *table}) {
    13           @{arg | *table}!(value) |
    14           ack!(value)
    15         }
    16       } |
    17       for(@"set", @arg1, @arg2, ack <= port) {
    18         for (_ <- @{arg1 | *table}) {
    19           @{arg1 | *table}!(arg2) |
    20           ack!(true)
    21         }
    22       }
    23     }
    24   } |
    25 
    26   // Usage
    27   new ret, get, set in {
    28     MakeCoatCheck!(*ret) |
    29     for (cc <- ret) {
    30       // Creates new cell with initial value 0
    31       cc!("new", 0, *ret) |
    32       for (ticket <- ret) {
    33         contract get(return) = { cc!("get", *ticket, *return) } |
    34         contract set(@value, return) = { cc!("set", *ticket, value, *return) } |
    35         
    36         get!(*ret) | for(@r <- ret) {
    37           //r is equal to 0
    38           set!(1, *ret) | for(_ <- ret) {
    39             get!(*ret) | for(@r <- ret) {
    40               //r is equal to 1
    41               Nil
    42             }
    43           }
    44         }
    45       }
    46     }
    47   }
    48 }

2) One design pattern, used in the MakeCell contract above, is to receive from the caller a channel for each different piece of functionality that a process provides.  An object-oriented programmer might say that MakeCell requires the caller to provide a channel for each method. MakeCoatCheck uses a more object-oriented approach, as we'll see.

3) We create a `port` channel for interacting with the coat check as well as a `table` name which will be used in storing/retrieving values in the coat check.

4) We send `port` out to the caller, so they can interact with the coat check.

5, 11, 17) We define different methods which can be called by sending a message on `port`. This is done by specifying mutually exclusive patterns the message on `port` can match, with the first element of the message being the method name and subsequent elements being the argument(s) and return channel. Using the `<=` arrow instead of the `<-` arrow means that the `for`s are "replicated". This gives them the same behavior as `contract`s, i.e. the process listening for messages on `port` persists after spawning an instance of its body.

8) We take advantage of being able to quote any process to make a name in order to create a unique name for each value to be stored at. The process `*ticket | *table` is produced by the concurrent composition of the processes produced by unquoting the names `ticket` and `table`. That process can then be quoted to form a unique name that is then used to store the value by sending it on the name.

## Dining philosophers and deadlock

     1 new philosopher1, philosopher2, north, south, knife, spoon in {
     2   north!(*knife) |
     3   south!(*spoon) |
     4   for (@knf <- north) { for (@spn <- south) {
     5     philosopher1!("Complete!") |
     6     north!(knf) |
     7     south!(spn)
     8   } } |
     9   for (@spn <- south) { for (@knf <- north) {
    10     philosopher2!("Complete!") |
    11     north!(knf) |
    12     south!(spn)
    13   } }
    14 }

The dining philosophers problem has two philosophers that share only one set of silverware.  Philosopher1 sits on the east side of the table while Philosopher2 sits on the west. Each needs both a knife and a spoon in order to eat.  Each one refuses to relinquish a utensil until he has used both to take a bite.  If both philosophers reach first for the utensil at their right, both will starve: Philosopher1 gets the knife, Philosopher2 gets the spoon, and neither ever lets go.

Here's how to solve the problem:

     1 new philosopher1, philosopher2, north, south, knife, spoon in {
     2   north!(*knife) |
     3   south!(*spoon) |
     4   for (@knf <- north; @spn <- south) {
     5     philosopher1!("Complete!") |
     6     north!(knf) |
     7     south!(spn)
     8   } |
     9   for (@spn <- south; @knf <- north) {
    10     philosopher2!("Complete!") |
    11     north!(knf) |
    12     south!(spn)
    13   }
    14 }

4, 9) The join operator, denoted with a semicolon `;`, declares that the continuation should only proceed if there is a message available on each of the channels simultaneously, preventing the deadlock above.

## Crypto channels

### Hashing

There are three hashing functions available:

- keccak256
- sha256
- blake2b256

Hashing functions are exposed as channels which expect two arguments:

- byte array to hash
- return channel for sending back the hash represented as byte array


#### Example usage:

```rholang
new x,y in {
    x!(@"name"!("Joe") | @"age"!(40)) |  // (1)
        for (@r <- x) {
            @"keccak256Hash"!(r.toByteArray(), *y) // hash the program from (1)
        } |
        for (@h <- y) {
            // The h here is the hash of the rholang term we sent to the hash channel.
            // We can do anything we want with it, but we choose to just print it.
            // Rholang prints byte arrays in hexadecimal.
            @"stdout"!(h)  // print out the keccak256 hash
        }
}
```


### Verify

1. Let's hash a rholang program and print out it in base16. In rholang:

  ```rholang
  new x,y in { 
     x!(@"name"!("Joe") | @"age"!(40)) |  // (1)
     for (@r <- x) { @"keccak256Hash"!(r.toByteArray(), *y) } |  // hash the program from (1)
     for (@h <- y) { @"stdout"!(h) }  // print out the keccak256 hash
  }
  ```

  This will print the hash of our program `(1)` : 
  `a6da46a1dc7ed715d4cd6472a736249a4d11142d160dbef9f20ae493de908c4e`

2. We need a pair of keys; let's generate some with `Ed25519`, available in the project. In the scala console, we enter the following:

  ```scala
  import coop.rchain.crypto.signatures._
  import coop.rchain.crypto.codec._

  val keyPair = Ed25519.newKeyPair
  val secKey = Base16.encode(keyPair._1)
  // secKey: String = f6664a95992958bbfeb7e6f50bbca2aa7bfd015aec79820caf362a3c874e9247
  val pubKey = Base16.encode(keyPair._2)
  // pubKey: String = 288755c48c3951f89c5f0ffe885088dc0970fd935bc12adfdd81f81bb63d6219
  ```

3.  Now we need to sign the hash we obtained in first step. First we convert the hexadecimal strings we printed earlier back into byte arrays, then sign the result:

  ```
  val signature = Ed25519.sign(Base16.decode("a6da46a1dc7ed715d4cd6472a736249a4d11142d160dbef9f20ae493de908c4e"), Base16.decode(secKey))
  val base16Repr = Base16.encode(signature)
  // d0a909078ce8b8706a641b07a0d4fe2108064813ce42009f108f89c2a3f4864aa1a510d6dfccad3b62cd610db0bfe82bcecb08d813997fa7df14972f56017e0b
  ```

4.  Now we can pass the signature and public key to our rholang program to verify it using the available crypto functions. 

  The `ed25519Verify` channel expects four arguments as follows:

  - data to verify. In our case, this will be the keccak256 hash of our rholang program. The hash is represented in base16, so we need to call `hexToBytes` on it to turn the string into byte array
  - signature. Again, we have hexadecimal string, so we need to turn it into a byte array with `hexToBytes`.
  - public key. This is the public key corresponding to the private one used to issue the signature. 
  - channel on which the result of verification will be returned.

  So, in rholang we run:
  ```
  new x in { 
    @"ed25519Verify"!("a6da46a1dc7ed715d4cd6472a736249a4d11142d160dbef9f20ae493de908c4e".hexToBytes(), "d0a909078ce8b8706a641b07a0d4fe2108064813ce42009f108f89c2a3f4864aa1a510d6dfccad3b62cd610db0bfe82bcecb08d813997fa7df14972f56017e0b".hexToBytes(),"288755c48c3951f89c5f0ffe885088dc0970fd935bc12adfdd81f81bb63d6219".hexToBytes(), *x) | 
    for (@v <- x) { @"stdout"!(v) } 
  } 

  ```
  and we should see: 
  ```
  @{true}
  ```

  which means that our signed hash is verified.

  If we, for example, pass in a corrupted hash, changing the initial 'a' to a 'b':
  ```
  new x in { 
     @"ed25519Verify"!("b6da46a1dc7ed615d4cd6472a736249a4d11142d160dbef9f20ae493de908c4e".hexToBytes(), "d0a909078ce8b8706a641b07a0d4fe2108064813ce42009f108f89c2a3f4864aa1a510d6dfccad3b62cd610db0bfe82bcecb08d813997fa7df14972f56017e0b".hexToBytes(),"288755c48c3951f89c5f0ffe885088dc0970fd935bc12adfdd81f81bb63d6219".hexToBytes(), *x) | 
     for (@v <- x) { @"stdout"!(v) } 
  } 
  ```

  we will get:
  ```
  @{false}
  ```

## Secure design patterns

In this section we describe several design patterns.  These patterns are adapted from Marc Stiegler's [_A PictureBook of Secure Cooperation_](http://erights.org/talks/efun/SecurityPictureBook.pdf).

### Facets

In the MakeCell contract, the client provides two channels, one for getting the value and one for setting it.  If the client then passes only the `get` channel to another process, that process effectively has a read-only view of the cell.  

Channels like `get` and `set` are called "facets" of the process.  They encapsulate the authority to perform the action.  If the `set` channel is a public channel like `@"Foo"`, then anyone who can learn or even guess the string `"Foo"` has the authority to set the cell's value.  On the other hand, if the `set` channel was created with the `new` operator, then there's no way for any other process to construct the `set` channel; it must be passed to a process directly in order for the process to use it.  

Note that possession of `get` and `set` is also authority to intercept messages sent to the cell:

    for (@ret <- get) { P } | 
    for (@ret <- get) { Q } | 
    get!(*ack)

This term has two processes listening on the channel `get` and a single message sent over `get`.  Only one of the two processes will be able to receive the message.

By receiving channels from the client for getting and setting, the MakeCell contract is leaving the decisions about how public those channels are to the client.  The MakeCoatCheck contract, on the other hand, constructs its own channels and exposes methods the client, so it is in a position to enforce privacy guarantees.

### Attenuating forwarders

In the MakeCoatCheck contract, there's only one channel and messages are dispatched internally.  To get the same effect as a read-only facet, we can create a forwarder process that simply ignores any messages it doesn't want to forward.  The contract below only forwards the "get" method.

    new MakeGetForwarder in {
      contract MakeGetForwarder(target, ret) = {
        new port in {
          ret!(*port) |
          contract port(@method, @arg, ack) = {
            match method == "get" { true => target!("get", arg, *ack) }
          }
        }
      }
    }
    
### Revocation

We can implement revocation by creating a forwarder with a kill switch.

     1 new MakeRevokableForwarder in {
     2   contract MakeRevokableForwarder(target, ret) = {
     3     new port, kill, forwardFlag in {
     4       ret!(*port, *kill) |
     5       forwardFlag!(true) |
     6       contract port(msg) = {
     7         for (@status <- forwardFlag) {
     8           forwardFlag!(status) |
     9           match status { true => target!(*msg) }
    10         }
    11       } |
    12       for (_ <- kill; _ <- forwardFlag) {
    13         forwardFlag!(false)
    14       }
    15     }
    16   }
    17 }

3) We create a port to listen for method calls and a channel `forwardFlag` to store whether to forward messages.

4) We return the channel on which clients send requests and the channel on which to send the kill signal.

5) We set the initial state of `forwardFlag` to true.

6-11) We read in an arbitrary message, get and replace the value of the flag.  If the flag is true, we forward the message to `target`.

12-14) If a message is ever sent on the `kill` channel, we set `forwardFlag` to false.  The forwarder process then stops forwarding messages.

### Composition

By combining an attenuating forwarder with a revokable forwarder, we get both features:

    new ret in {
      MakeGetForwarder(target, ret) |
      for (@pair <- ret) {
        match pair {
          [getOnly, kill] => {
            MakeRevokableForwarder!(getOnly, *ret) |
            for (revokableGetOnly <- ret) {
              // give away revokableGetOnly instead of target
              // hang onto kill for later revocation
            }
          }
        }
      }
    }

### Logging forwarder

A logging forwarder can record all messages sent on a channel by echoing them to a second channel.

    contract MakeLoggingForwarder(target, logger, ret) = {
      new port in {
        ret!(*port) |
        contract port(@msg) = {
          target!(msg) |
          logger!(msg)
        }
      }
    }

### Accountability

Suppose Alice has a channel and would like to log Bob's access to it.  Bob would like to delegate the use of that channel to Carol and log her access.  Each party is free to construct their own logging forwarder around the channel they have received.  Alice will hold Bob responsible for whatever Carol does.

### Sealing and unsealing

    contract MakeSealerUnsealer(ret) =  {
      new sealer, unsealer, ccRet in {
        ret!(*sealer, *unsealer) |
        MakeCoatCheck!(*ccRet) |
        for (cc <- ccRet) {
          contract sealer(@value, ret) = {
            cc!("new", value, *ret)
          } |
          contract unsealer(@ticket, ret) = {
            cc!("get", ticket, *ret)
          }
        }
      }
    }


A sealer/unsealer pair gives the same functionality as public keys, but without cryptography. It's merely an attenuation of the coat check described above.  This design pattern can be used to sign something on a user's behalf.  In the Rholang blockchain tutorial, we'll see that a sealer/unsealer pair even works as a signing/verification pair of keys on the blockchain because there are no secrets to store, only unforgeable names to be kept inaccessible.

### Beware of sending attenuators

A basic principle to keep in mind with RChain processes is one that is similar to more traditional web applications: whatever code you send to another party can be disassembled.  Ever since the late 1990s when buying things over the web became possible, [there have been e-commerce platforms](https://blog.detectify.com/2016/11/17/7-most-common-e-commerce-security-mistakes/) where the platform relied on the users' browsers to send the correct price of the item back to it.  The authors didn't think about the user opening the developer tools and changing the price before it got sent back.  The right way to build an e-commerce platform is to store the prices on the server and check them there.

Suppose that Bob is willing to run some code for Alice; he has a contract that says something like, "Get a process from this channel and run it."

    for (p <- x) { *p }

This is just like a web browser being willing to run the JavaScript code it gets from a website.  If Alice sends Bob an attenuating forwarder, Bob can use the pattern matching productions in Rholang to take apart the process and get access to the underlying resource.  Instead, like in the e-commerce example, Alice should only send code that forwards requests to her own processes and do the attenuation there.

## Conclusion

RChain is a language designed for use on a blockchain, but we have not mentioned anything about nodes, namespaces, wallets, Rev and phlogiston, network structure, or Casper.  A forthcoming document will address all these issues and more.

We hope that the foregoing examples spark a desire to write more code and demonstrate the ease of expressing concurrent designs.
