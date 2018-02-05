# A Rholang tutorial

Rholang is a new programming language designed for use in distributed systems.  Like all newborn things, it is growing and changing rapidly; this document describes the syntax that will be used in the 0.1 SDK release.

Rholang is "process-oriented": all computation is done by means of message passing.  Messages are passed on "channels", which are rather like message queues but behave like sets rather than queues.  Rholang is completely asynchronous, in the sense that while you can read a message from a channel and then do something with it, you can't send a message and then do something once it has been received---at least, not without explicitly waiting for an acknowledgement message from the receiver.

## Contracts and sending data

    1 new helloWorld in {
    2   contract helloWorld(name) = {
    3     "Hello, ".display(name, "!\n")
    4   } |
    5   helloWorld("Joe")
    6 }

1) A Rholang program is a single process.  This process starts by creating a new channel named `helloWorld`.  To create a new, private channel, we use the `new ... in` construction. No other process can send or receive messages over this channel unless we explicitly send this channel to the other process.

2) The `contract` production creates a process that spawns a copy of its body whenever it receives a message.

3) The `display` method of a string writes to standard out.  It takes a list of strings to print next. Therefore, for this to work, the message `name` should be a string.

5) We send the string `"Joe"` over the channel `helloWorld`.

## Receiving data

     1 new helloAgain in {
     2   contract helloAgain(_) = {
     3     new chan in {
     4       chan("Hello again, world!") |
     5       for (text <- chan) {
     6         text.display("\n")
     7       }
     8     }
     9   } |
    10   helloAgain(Nil)
    11 }

2) Contracts take at least one parameter, but we can throw it away by binding it to a variable we never use.

3) We create a new channel `chan`.

4) We send the string process `"Hello again, world!"` over the new channel.

5) We listen on the new channel for a single message.  The `for` operation blocks until there's a message available on the channel `chan`. The `for` operation is just like a contract except that it only reads one message and then becomes its body instead of forking a copy of its body for each message.

## Mutable state

     1 new MakeCell in {
     2   // Makes a single cell in which you can store values
     3   contract MakeCell(init, get, set) = {
     4     new valueStore in {
     5       valueStore(init) |
     6       contract get(ack) = {
     7         for(value <- valueStore) {
     8           valueStore(value) | ack(value)
     9         }
    10       } |
    11       contract set(pair) = {
    12         for(_ <- valueStore) {
    13           match pair with [newValue, ack] => {
    14             valueStore(newValue) | ack(Nil)
    15           }
    16         }
    17       }
    18     }
    19   } |
    20   // Cell usage.
    21   new myGet, mySet in {
    22     MakeCell(123, myGet, mySet) |
    23     new ack in {
    24       myGet(ack) |
    25       for (result <- ack) {
    26         result.display("\n") |
    27         mySet([456, ack]) |
    28         for (_ <- ack) {
    29           myGet(ack) |
    30           for (result <- ack) {
    31             result.display("\n")
    32           }
    33         }
    34       }
    35     }
    36   }
    37 }

1) We create a new channel MakeCell and then use it on line 3 as the name of an internal contract.  No process other than the code inside this lexical scope can invoke it.

3) The `MakeCell` contract takes three arguments.  The first argument is the initial value to be stored in the cell.  The second and third arguments are channels over which the cell will receive requests to get and set the value.

4) To store the value, we create a new channel.  This channel will have at most one message on it containing the current value of the cell.  

5) Before this line, there are no messages on the `valueStore` channel.  After we send the initial value, it is the only value on that channel.

6) We set up a contract to listen on the `get` channel.  Each time a message is sent on `get`, the body of the contract will be executed

7) We block until we get one message from the `valueStore` channel.  Because there is at most one message ever waiting on `valueStore`, reading the message behaves much like acquiring a lock.

8) We send the current value on `valueStore` again, allowing other messages to be processed, and we send the current value back to the client on the `ack` channel.

11) In parallel with the `get` contract, we run a contract listening on `set`.

12) We block until there's a message on `valueStore`, then read it.  We throw away the message that we read.

13) The `match` operation does destructuring bind, splitting up the tuple `pair` into its components and assigning names to them.

14) We send the new value to store on `valueStore` and signal that the operation is complete.

21-36) The usage code demonstrates creating a cell, assigning the initial value 123, getting and printing that value, setting the value to 456, then getting and printing that value.  

Note the deep layers of callback. Rholang was designed to make parallel computations natural to express; as a consequence, data dependencies implicit in sequencing in other languages must be made explicit.

## Iteration and matching

In the code below, `iterate` first sends a channel `next` over `iterator`, and then for each message received on `next` sends a pair containing the next item in the list and whether the iteration is done.

     1 new iterator, iterate in {
     2     contract iterate(list, iterator) = {
     3         new next, right in {
     4             iterator(next) |
     5             for (_ <- next) {
     6                 contract right(pair) = {
     7                     match pair with [i, limit] => {
     8                         iterator([list.nth(i), i < limit]) |
     9                         for (_ <- next) {
    10                             match i + 1 < limit with true => {
    11                                 right([i + 1, limit]) 
    12                             }
    13                         }
    14                     }
    15                 } |
    16                 right([0, list.size()])
    17             }
    18         }
    19     } |
    20     // Invoke the iterator contract on channel
    21     iterate([4,5,6], iterator) |
    22     
    23     // Interacts with the iterator
    24     for (next <- iterator) {
    25         next(Nil) |
    26         new left in {
    27             contract left(_) = {
    28                 for (pair <- iterator) {
    29                     match pair with [v, keepGoing] => {
    30                         v.display("\n") |
    31                         match keepGoing with true => { 
    32                             next(Nil) |
    33                             left(Nil) 
    34                         }
    35                     }
    36                 }
    37             } |
    38             left(Nil)
    39         }
    40     }
    41 }

7) The `match .. with` construction allows destructuring bind.

8) The `nth` method on tuples allows extracting individual elements.

16) Tuples have a `size` method.

## Maps

     1 new MakeCoatCheck in {
     2     contract MakeCoatCheck(ret) = {
     3         new port, mapStore in {
     4             mapStore(Map()) |
     5             ret(port) |
     6             contract port (method, ack, arg1, arg2) = {
     7                 match method with
     8                 "new" => {
     9                     for (map <- mapStore) {
    10                         new ticket in {
    11                             map.insert(ticket, arg1) |
    12                             mapStore(map) |
    13                             ack(ticket)
    14                         }            
    15                     }
    16                 }
    17                 "get" => {
    18                     for (map <- mapStore) {
    19                         mapStore(map) |
    20                         ack(map.get(arg1))
    21                     }
    22                 }
    23                 "set" => {
    24                     for (map <- mapStore) {
    25                         map.insert(arg1, arg2) |
    26                         mapStore(map) |
    27                         ack(Nil)
    28                     }
    29                 }
    30             }
    31         }
    32     } |
    33 
    34     // Usage
    35     new ret in {
    36         MakeCoatCheck(ret) |
    37         for (cc <- ret) {
    38             // Creates new cell with initial value 0
    39             cc("new", ret, 0, Nil) |
    40             for (ticket <- ret) {
    41                 // Sets the cell to 1
    42                 cc("set", ret, ticket, 1) |
    43                 for (ack <- ret) {
    44                     // Reads the value
    45                     cc("get", ret, ticket, Nil) |
    46                     for (storedValue <- ret) {
    47                         // Prints 1
    48                         storedValue.display("\n")
    49                     }
    50                 }
    51             }
    52         }
    53     }
    54 }

2) One design pattern, used in the MakeCell contract above, is to receive from the caller a channel for each different piece of functionality that a process provides.  An object-oriented programmer might say that MakeCell requires the caller to provide a channel for each method.  Matches are attempted in the order they appear in the code; if no match occurs, the `match` block evaluates to the `Nil` process.  MakeCoatCheck uses a more object-oriented approach, as we'll see.

3-4) Each coat check has its own mutable reentrant map in which to store items.  We store the newly constructed map on mapStore.  It has the following API:

    insert(key, value)
    insertMany(key1, val1, key2, val2, ..., keyn, valn)
    getOrElse(key, default)
    get(key)

6) We expect four arguments every time; we could also have expected a single tuple and used destructuring bind to dispatch based on both the method and the tuple's length.

## Dining philosophers and deadlock

     1 new north, south, knife, spoon in {
     2     north(knife) |
     3     south(spoon) |
     4     for (knf <- north) { for (spn <- south) {
     5         "Philosopher 1 Utensils: ".display(knf, ", ", spn, "\n") |
     6         north(knf) |
     7         south(spn)
     8     } } |
     9     for (knf <- north) { for (spn <- south) {
    10         "Philosopher 2 Utensils: ".display(knf, ", ", spn, "\n") |
    11         north(knf) |
    12         south(spn)
    13     } }
    14 }

The dining philosophers problem has two philosophers that share only one set of silverware.  Philosopher1 sits on the east side of the table while Philosopher2 sits on the west. Each needs both a knife and a fork in order to eat.  Each one refuses to relinquish a utensil until he has used both to take a bite.  If both philosophers reach first for the utensil at their right, both will starve: Philosopher1 gets the knife, Philosopher2 gets the fork, and neither ever lets go.

Here's how to solve the problem:

     1 new north, south, knife, spoon in {
     2     north(knife) |
     3     south(spoon) |
     4     for (knf <- north; spn <- south) {
     5         "Philosopher 1 Utensils: ".display(knf, ", ", spn, "\n") |
     6         north(knf) |
     7         south(spn)
     8     } |
     9     for (spn <- south; knf <- north) {
    10         "Philosopher 2 Utensils: ".display(knf, ", ", spn, "\n") |
    11         north(knf) |
    12         south(spn)
    13     }
    14 }

4, 9) The join operator, denoted with a semicolon `;`, declares that the continuation should only proceed if there is a message available on each of the channels simultaneously, preventing the deadlock above.

## Secure design patterns

In this section we describe several design patterns.  These patterns are adapted from Marc Stiegler's [_A PictureBook of Secure Cooperation_](http://erights.org/talks/efun/SecurityPictureBook.pdf).

### Facets

In the MakeCell contract, the client provides two channels, one for getting the value and one for setting it.  If the client then passes only the `get` channel to another process, that process effectively has a read-only view of the cell.  

Channels like `get` and `set` are called "facets" of the process.  They encapsulate the authority to perform the action.  If the `set` channel is a public channel like `@"Foo"`, then anyone who can learn or even guess the string `"Foo"` has the authority to set the cell's value.  On the other hand, if the `set` channel was created with the `new` operator, then there's no way for any other process to construct the `set` channel; it must be passed to a process directly in order for the process to use it.  

Note that if `get` and `set` are not created as halves of iopairs, then possession of those channels is also authority to intercept messages sent to the cell:

    for (ret <- get) { P } | 
    for (ret <- get) { Q } | 
    get(ack)

This term has two processes listening on the channel `get` and a single message sent over `get`.  Only one of the two processes will be able to receive the message.

By receiving channels from the client for getting and setting, the MakeCell contract is leaving the decisions about how public those channels are to the client.  The MakeCellFactory contract, on the other hand, constructs its own channels and returns them to the client, so it is in a position to enforce privacy guarantees.

### Attenuating forwarders

In the MakeCellFactory contract, there's only one channel and messages are dispatched internally.  To get the same effect as a read-only facet, we can create a forwarder process that simply ignores any messages it doesn't want to forward.  The contract below only forwards the "get" method.

    contract MakeGetForwarder(target, ret) = {
        new port in {
            ret(port) |
            contract port(tuple) = {
                tuple.nth(0) match with "get" => target(tuple)
            }
        }
    }

### Revocation

We can implement revocation by creating a forwarder with a kill switch.

     1 contract MakeRevokableForwarder(target, ret) = {
     2     new port, kill, forwardFlag in {
     3         ret(port, kill) |
     4         forwardFlag(true) |
     5         contract port(tuple) = {
     6             for (status <- forwardFlag) {
     7                 forwardFlag(status) |
     8                 match status with true => { target(tuple) }
     9             }
    10         } |
    11         for (_ <- kill; _ <- forwardFlag) {
    12             forwardFlag(false)
    13         }
    14     }
    15 }

2) We create a port to listen for method calls and a channel `forwardFlag` to store whether to forward messages.

3) We return the channel on which clients send requests and the channel on which to send the kill signal.

4) We set the initial state of `forwardFlag` to true.

5-10) We read in an arbitrary tuple of message parts and get and replace the value of the flag.  If the flag is true, we forward the message tuple to `target`.

11-13) If a message is ever sent on the `kill` channel, we set `forwardFlag` to false.  The forwarder process then stops forwarding messages.

### Composition

By combining an attenuating forwarder with a revokable forwarder, we get both features:

    new ret in {
        MakeGetForwarder(target, ret) |
        for (pair <- ret) {
            match pair with [getOnly, kill] => {
                MakeRevokableForwarder(getOnly, ret) |
                for (revokableGetOnly <- ret) {
                    // give away revokableGetOnly instead of target
                    // hang onto kill for later revocation
                }
            }
        }
    }

### Logging forwarder

A logging forwarder can record all messages sent on a channel by echoing them to a second channel.

    contract MakeLoggingForwarder(target, logger, ret) = {
        new port in {
            ret(port) |
            contract port(tuple) {
                target(tuple) |
                logger(tuple)
            }
        }
    }

### Accountability

Suppose Alice has a channel and would like to log Bob's access to it.  Bob would like to delegate the use of that channel to Carol and log her access.  Each party is free to construct their own logging forwarder around the channel they have received.  Alice will hold Bob responsible for whatever Carol does.

### Sealing and unsealing

    contract MakeSealerUnsealer(ret) =  {
        new sealer, unsealer, ccRet in {
            ret(sealer, unsealer) |
            MakeCoatCheck(ccRet) |
            for (cc <- ccRet) {
                contract sealer(value, ret) = {
                    cc("new", ret, value, Nil)
                } |
                contract unsealer(ticket, ret) = {
                    cc("get", ret, ticket, Nil)
                }
            }
        }
    }


A sealer/unsealer pair gives the same functionality as public keys, but without cryptography. It's merely an attenuation of the coat check described above.  This design pattern can be used to sign something on a user's behalf.  In the Rholang blockchain tutorial, we'll see that it even works on the blockchain because there are no secrets to store, only unforgeable names to be kept inaccessible.

### Beware of sending attenuators

A basic principle to keep in mind with RChain processes is one that is similar to more traditional web applications: whatever code you send to another party can be disassembled.  Ever since the late 1990s when buying things over the web became possible, [there have been e-commerce platforms](https://blog.detectify.com/2016/11/17/7-most-common-e-commerce-security-mistakes/) where the platform relied on the users' browsers to send the correct price of the item back to it.  The authors didn't think about the user opening the developer tools and changing the price before it got sent back.  The right way to build an e-commerce platform is to store the prices on the server and check them there.

Suppose that Bob is willing to run some code for Alice; he has a contract that says something like, "Get a process from this channel and run it."

    for (p <- x) { *p }

This is just like a web browser being willing to run the JavaScript code it gets from a website.  If Alice sends Bob an attenuating forwarder, Bob can use the pattern matching productions in Rholang to take apart the process and get access to the underlying resource.  Instead, like in the e-commerce example, Alice should only send code that forwards requests to her own processes and do the attenuation there.

## Conclusion

RChain is a language designed for use on a blockchain, but we have not mentioned anything about nodes, namespaces, wallets, Rev and phlogiston, network structure, or Casper.  A forthcoming document will address all these issues and more.

We hope that the foregoing examples spark a desire to write more code and demonstrate the ease of expressing concurrent designs.