# A Rholang tutorial

Rholang is a new programming language designed for use in distributed systems.  Like all newborn things, it is growing and changing rapidly; this document describes the syntax that will be used in the Mercury release.

Rholang is "process-oriented": all computation is done by means of message passing.  Messages are passed on "channels", which are rather like message queues but behave like sets rather than queues.  Rholang is completely asynchronous, in the sense that while you can read a message from a channel and then do something with it, you can't send a message and then do something once it has been received---at least, not without explicitly waiting for an acknowledgement message from the receiver.

## Contracts, reflection, and sending data

    1 contract @"HelloWorld"(system) = {
    2     system!("print", "Hello, world!")
    3 }

1) On the internet, servers have IP addresses.  The Domain Name Service (DNS) maps alphanumeric strings to numbers, rather like a phone book.  Instead of using numbers or strings, Rholang is "reflective": all channels are named by a serialized process.  All serializations of processes start with an `@`.  This contract listens for messages sent on the channel named by the serialization of the string process `"HelloWorld"`.  To be concise, we say, "This contract listens on the name `@"HelloWorld"`."

A contract declares an API by which other processes can interact with it.  Top-level contracts all have the same API: they have one argument, the channel for the `system` process.  The `system` process contains the names of all the channels that can cause side-effects on the node. It is a built-in process that listens for messages consisting of a method name and some arguments.

2) The exclamation point operator sends the message on its right over the channel on its left.  Each message is a tuple of names.  If we send a process instead of a name, it automatically gets serialized into a name.

In this case, we're sending a message consisting of two processes, the string `"print"` and the string `"Hello, world!"`.  The `system` process is a built-in process that listens for messages consisting of a method name and some arguments; in this case, the process echoes the second argument to standard output.

## New channels, receiving data, and patterns

    1 contract @"HelloAgain"(system) = new chan in {
    2     chan!("Hello again, world!") |
    3     for (@text <- chan) system!("print", text)
    4 }

1) To create a new, private channel, we use the `new ... in` construction. No other process can send or receive messages over this channel unless we explicitly send this channel to the other process.

2) We send the string process `"Hello again, world!"` over the new channel.

3) We listen on the new channel for a message.  The `for` operation blocks until there's a message available on the channel `chan`. 

The language of patterns (also known as "spatial types") includes the language of processes and names.  Every process with free variables is a pattern that matches a process with the same structure; the free variables bind to the subprocesses in that position.  On line 3, we expect a tuple with one element.  That element is the serialization of some process, and therefore starts with an `@`.  Since we sent a string on line 2, the free variable`text` gets bound to that string process.  Finally, line 3 forwards that string to the `system` process to print.

Later, we'll explore some more features of patterns.

## Mutable state, replication, and select

     1 contract @"CellDemo"(system) = new MakeCell in {
     2     // Makes a single cell in which you can store values
     3     contract MakeCell(@init, get, set) = new valueStore in {
     4         valueStore!(init) |
     5         for (@value <= valueStore) {
     6             select {
     7                 ack <- get => valueStore!(value) | ack!(value)
     8                 @newValue, ack <- set => valueStore!(newValue) | ack!()
     9             }
    10         }
    11     } |
    12 
    13     // Cell usage.
    14     new myGet, mySet in {
    15         MakeCell(123, *myGet, *mySet) |
    16         new ack in {
    17             myGet!(*ack) |
    18             for (@result <- ack) {
    19                 system!("print", result, *ack) |
    20                 for (_ <- ack) {
    21                     mySet!(456, *ack) |
    22                     for (_ <- ack) {
    23                         myGet!(*ack) |
    24                         for (@result <- ack) {
    25                             system!("print", result)
    26                         }
    27                     }
    28                 }
    29             }
    30         }
    31     }
    32 }

1) We create a new channel MakeCell and then use it on line 3 as the name of an internal contract.  No process other than the code inside the `@"CellDemo"` contract can invoke it.

3) The `MakeCell` contract takes three arguments.  The first argument is a process, the initial value to be stored in the cell.  The second and third arguments are channels over which the cell will receive requests to get and set the value.  Those familiar with C++ can think of channels as roughly equivalent to pointers in the sense that a pointer is a serializable data type that denotes a location.  The operator `@` in this context as roughly equivalent to the `&` that denotes pass-by-reference; the variable `init` gets bound to a process instead of a channel.  In C++, to convert a pointer to a reference, one uses the `*` operator; similarly, in Rholang to convert a channel to a process, we use the `*` operator.  Since we can only send processes over a channel, the `*` operator is used very often; see the odd-numbered lines in the usage example.

To store the value, we create a new channel.  This channel will have at most one message on it containing the current value of the cell.

4) Before this line, there are no messages on the `valueStore` channel.  After we send the initial value, it is the only value on that channel.

5) In parallel with 4, we try to read from `valueStore`.  The double-stemmed arrow says that once we get a message on that channel, we should spawn a copy of the process immediately after the `for` and start trying to read on that channel again immediately.

Once a message becomes available, we bind the variable `value` to the process in the message.

6-8) The `select` keyword on line 6 means that only one of the branches on lines 7 or 8 will be able to proceed.  At this point, there is no message waiting on the `valueStore` channel.

If there is a message on the channel `get`, then the branch on line 7 may run.  The variable `ack` gets bound to the message, and then, in parallel, two things occur: the value we read gets sent again on `valueStore` and also sent on the channel `ack`.

Messages are tuples of names.  All the messages we've seen so far have had arity 1, but here on line 8, we are waiting for a message of arity 2.  The first part of the pattern binds the variable `newValue` to the first part of the message, while the second part of the pattern binds the variable `ack` to a channel.  Instead of sending `value` on `valueStore` like we did in line 7, we send `newValue`.

Also on line 8, we send a message of arity 0.  In the usage example, lines 20 and 22 use an underscore to receive that empty message and discard it.

9) At this point, there is exactly one message on the `valueStore` channel again.

13-31) The usage code demonstrates creating a cell, assigning the initial value 123, getting and printing that value, setting the value to 456, then getting and printing that value.  

Note the deep layers of callback. Rholang was designed to make parallel computations natural to express; as a consequence, data dependencies implicit in sequencing in other languages must be made explicit.

## Iteration 

    1 contract @"IterateDemo"(system) = new chan in {
    2     [1,2,3].iterate(chan) |
    3     for (@num, ack << chan) system!("print", num, *ack)
    4 }

2) Brackets indicate a list.  Lists are mutable, while tuples, indicated with parentheses, are not.  Some processes, like those backed by Java objects, have methods; here, the `iterate` method is being invoked with a channel to iterate on.

3) The `<<` operator indicates "sequential send", or in other words, that messages from chan need acknowledgement before new messages will be sent.  

The `print` method of the system process can take one or two arguments.  In the two-parameter case, the second argument is a channel on which an acknowledgement message will be sent once the printing has completed.  The `iterate` method will receive that acknowledgement and send the next message.  Once the list has finished iterating, line 3 will evolve to the `Nil` process that does nothing and gets garbage collected.

## Pattern matching and rest parameters

     1 contract @"CoatCheckDemo"(system) = new MakeCoatCheck in {
     2     contract MakeCoatCheck(ret) = {
     3         new (portIn, portOut):iopair, table in {
     4             ret!(*portOut) |
     5             for (@method, ack, ...@rest <= portIn) {
     6                 match method {
     7                     case "new" => match rest {
     8                         case (initialValue) => new ticket in {
     9                             ack!(*ticket) |
    10                             @(*ticket | *table)!(initialValue)
    11                         }            
    12                     }
    13                     case "get" => match rest {
    14                         case (ticket) => {
    15                             for (@value <! @(*ticket | *table)) {
    16                                 ack!(value)
    17                             }
    18                         }
    19                     }
    20                     case "set" => match rest {
    21                         case (store, @newValue) => {
    22                             for (_ <- @(*ticket | *table)) {
    23                                 @(*ticket | *table)!(newValue) |
    24                                 ack!()
    25                             }
    26                         }
    27                     }
    28                 }
    29             }
    30         }
    31     } |
    32 
    33     // Usage
    34     new ret in {
    35         MakeCoatCheck(ret) |
    36         for (cc <- ret) {
    37             // Creates new cell with initial value 0
    38             cc!("new", *ret, 0) |
    39             for (ticket <- ret) {
    40                 // Sets the cell to 1
    41                 cc!("set", *ret, *ticket, 1) |
    42                 for (ack <- ret) {
    43                     // Reads the value
    44                     cc!("get", *ret, *ticket) |
    45                     for (@storedValue <- ret) {
    46                         // Prints 1
    47                         system!("print", storedValue)
    48                     }
    49                 }
    50             }
    51         }
    52     }
    53 }

2) One design pattern, used in the MakeCell contract above, is to receive from the caller a channel for each different piece of functionality that a process provides.  An object-oriented programmer might say that MakeCell requires the caller to provide a channel for each method.  Matches are attempted in the order they appear in the code; if no match occurs, the `match` block evaluates to the `Nil` process.

3) MakeCoatCheck uses a more object-oriented approach.  The `(in, out):iopair` construction lets us create a coupled pair of channels.  It is a type error to send a message over the `in` channel or to receive a message over the `out` channel.  However, any message sent over the `out` channel can be received over the `in` channel.  This allows us to return the `out` channel on which "method calls" can be made without allowing other processes to intercept requests meant for our process.

The new channel `table` will be used to create channels for internal use.

5) We repeatedly read in messages here of any arity greater than or equal to two.  The variable `method` gets bound to the first argument; we expect a string that names the method, just like the system process does.  The variable `ack` gets bound to a channel on which we will send any result of the method call.  The variable `rest` gets bound to a tuple containing the remainder of the message parts.

6) The `match ... case` construction lets us pattern match on the structure of a process.  We use that to do message dispatch.

7-12) If `method` is the string `"new"`, then we assume on line 8 that `rest` will be a tuple with one element, the initial value.  We create a `ticket` channel and return it using the `ack` channel.  We also combine the processes `*ticket` with `*table` into a new process and then derive a channel name from them.  Since only we have access to `table`, only we can manipulate the data stored on channels constructed this way.  The channel `ticket` behaves like a redeemable ticket for a checked coat, and the channel `@(*ticket | *table)` is the table entry indicated by that key.

13-19) If `method` is the string `"get"`, then we assume on line 14 that `rest` will be a tuple with one element, the particular ticket for the item to fetch.  The `<!` operator reads a value from the table and immediately puts it back; that is,

    for (y <! x) { P }

is syntactic sugar for

    for (y <- x) { x!(*y) | P }.

20-27) If `method` is the string `"set"`, then we assume on line 21 that `rest` will be a tuple with two elements: the key and the new value.  Line 22 throws away the current value at that ticket, line 23 sends the new value, and line 24 signals that it's done.

## Error handling

     1 for (@info, ret, err <- channel) {
     2     // Either return a result on ret or an error on err
     3 } |
     4 select {
     5     result <- ret => {
     6         // Process result
     7     }
     8     // Messages on err that don't fit this pattern
     9     // aren't intercepted here.
    10     @"TypeError", msg <- err => {
    11         // Handle type error
    12     }
    13 }

1-3) We can specify multiple channels on which data can be sent back to a client.

4) In this context, `select` behaves much like `try` in other languages.   Only one of the receives on lines 5 and 10 will proceed; they race to see which one gets a message first.  If lines 1-3 have an invariant that either a result is sent on `ret` or an error on `err`, then there will not be a race.  If, on the other hand, we want to send both a result and an error, we should use `for` instead:

    for (@info, ret, err <- channel) {
        // Either return a result on ret or an error on err
    } |
    for(result <- ret) {
        // Process result
    } |
    for(@"TypeError", msg <- err) {
        // Handle type error
    }

10) The pattern here is more complicated that ones we've seen before.  Here, we specify that we only want messages with two names, and the first name should be the serialization of the string `"TypeError"`.  If it's anything else, this branch won't proceed.

## Dining philosophers and deadlock

     1 new north, south, knife, fork in {
     2     north!(knife) |
     3     south!(fork) |
     4     for (knf <- north) for (frk <- south) {
     5         philosopher1!(knf, frk)
     6     } |
     7     for (frk <- south) for (knf <- north) {
     8         philosopher2!(knf, frk)
     9     }
    10 }

The dining philosophers problem has two philosophers that share only one set of silverware.  Philosopher1 sits on the east side of the table while Philosopher2 sits on the west. Each needs both a knife and a fork in order to eat.  Each one refuses to relinquish a utensil until he has used both to take a bite.  If both philosophers reach first for the utensil at their right, both will starve: Philosopher1 gets the knife, Philosopher2 gets the fork, and neither ever lets go.

Here's how to solve the problem:

     1 new north, south, knife, spoon in {
     2     north!(knife) |
     3     south!(spoon) |
     4     for (knf <- north; spn <- south) {
     5         philosopher1!(knf, spn)
     6     } |
     7     for (spn <- south; knf <- north) {
     8         philosopher2!(knf, spn)
     9     }
    10 }

4, 7) The join operator, denoted with a semicolon `;`, declares that the continuation should only proceed if there is a message available on each of the channels simultaneously, preventing the deadlock above.

## Built-in types, permanent send, logical connectives, and filtering

We've seen that the patterns we can use in the `match` construction or in the `for` construction include processes with free variables.  We can also use patterns that describe builtin processes.  The pattern `Integer` describes all 32-bit signed integers; similarly for `Double`, `String`, and `Boolean`.

We can combine patterns using the logical connectives AND, OR, and NOT, denoted `&&`, `||`, and `~`, respectively.

    for (@(x && Integer) <- y) { P }

This process binds a process variable `x` to the message received on `y`, but it also insists that `x` is an integer.

There's an American riddle that says, "How do you make fifteen cents in change when one coin is not a nickel and the other is not a dime?"  A nickel is worth five cents and a dime is worth ten.  Suppose that we have the following messages sent on `coins`, encoding the available kinds of American coin with value less than fifteen cents:

    coins!!(1) | coin!!(5) | coin!!(10)

The `!!` operator means that the messages should remain on the channel permanently and not be consumed when received by a `for` construction.

    new x in { x!!("Hi there!") | for (msg <= x) { system!("print", msg) } }

The process above will print "Hi there!" for as long as the virtual machine is running.

We can encode the riddle as

    for (@(x && ~5) <- coins; @(y && ~10) <- coins if x+y == 15) {
        system!("print", (x, y))
    }

The first pattern, `@(x && ~5)`, will match any message on `coins` that is not 5 and bind `x` to it; similarly, the second pattern `@(y && ~10)` will match any message on `coins` that is not 10 and will bind `y` to it.  The `if` clause in a `for` construction only allows the match to proceed if the formula to the right of it evaluates to `true`; in this case, nothing will be printed unless the values sum to 15.  

The answer to the riddle is, "A dime and a nickel", since a dime is not a nickel and a nickel is not a dime.  The variable `x` will bind to 10 and the variable `y` will bind to 5, and `(10, 5)` will be printed.

## Secure design patterns

In this section we describe several design patterns.  These patterns are adapted from Marc Stiegler's [_A PictureBook of Secure Cooperation_](http://erights.org/talks/efun/SecurityPictureBook.pdf).

### Facets

In the MakeCell contract, the client provides two channels, one for getting the value and one for setting it.  If the client then passes only the `get` channel to another process, that process effectively has a read-only view of the cell.  

Channels like `get` and `set` are called "facets" of the process.  They encapsulate the authority to perform the action.  If the `set` channel is a public channel like `@"Foo"`, then anyone who can learn or even guess the string `"Foo"` has the authority to set the cell's value.  On the other hand, if the `set` channel was created with the `new` operator, then there's no way for any other process to construct the `set` channel; it must be passed to a process directly in order for the process to use it.  

Note that if `get` and `set` are not created as halves of iopairs, then possession of those channels is also authority to intercept messages sent to the cell:

    for (ret <- get) { P } | 
    for (ret <- get) { Q } | 
    get!(*ack)

This term has two processes listening on the channel `get` and a single message sent over `get`.  Only one of the two processes will be able to receive the message.

By receiving channels from the client for getting and setting, the MakeCell contract is leaving the decisions about how public those channels are to the client.  The MakeCellFactory contract, on the other hand, constructs its own channels and returns them to the client, so it is in a position to enforce privacy guarantees.

### Attenuating forwarders

In the MakeCellFactory contract, there's only one channel and messages are dispatched internally.  To get the same effect as a read-only facet, we can create a forwarder process that simply ignores any messages it doesn't want to forward.  The contract below only forwards the "get" method.

    contract MakeGetForwarder(target, ret) = {
        new (portIn, portOut):iopair in {
            ret!(*portOut) |
            for (@method, ...@rest <= portIn) {
                method match {
                    case "get" => target!(method, ...rest)
                }
            }
        }
    }

### Revocation

We can implement revocation by creating a forwarder with a kill switch.

     1 contract MakeRevokableForwarder(target, ret) = {
     2     new (portIn, portOut):iopair, kill, forwardFlag in {
     3         ret!(*portOut, *kill) |
     4         forwardFlag!(true) |
     5         for (...@rest <= portIn) {
     6             for (@status <! forwardFlag) {
     7                 if (status) {
     8                     target!(rest)
     9                 } else {
    10                     Nil
    11                 }
    12             }
    13         } |
    14         for (_ <- kill; _ <- forwardFlag) {
    15             forwardFlag!(false)
    16         }
    17     }
    18 }

2) We create an iopair for method dispatch and a channel `forwardFlag` to store whether to forward messages.

3) We return the channel on which clients send requests and the channel on which to send the kill signal.

4) We set the initial state of `forwardFlag` to true.

5-13) We read in an arbitrary tuple of message parts and get the value of the flag.  If the flag is true, we forward the message tuple to `target`.

14-15) If a message is ever sent on the `kill` channel, we set `forwardFlag` to false, which stops forwarding messages.

### Composition

By combining an attenuating forwarder with a revokable forwarder, we get both features:

    new ret in {
        MakeGetForwarder(target, *ret) |
        for (@getOnly, kill <- ret) {
            MakeRevokableForwarder(getOnly, *ret) |
            for (@revokableGetOnly <- ret) {
                // give away revokableGetOnly instead of target
                // hang onto kill for later revocation
            }
        }
    }

### Logging forwarder

A logging forwarder can record all messages sent on a channel by echoing them to a second channel.

    contract MakeLoggingForwarder(target, logger, ret) = {
        new (portIn, portOut):iopair in {
            ret!(*portOut) |
            for (...@rest <= portIn) {
                target!(...rest) |
                logger!(...rest)
            }
        }
    }

### Accountability

Suppose Alice has a channel and would like to log Bob's access to it.  Bob would like to delegate the use of that channel to Carol and log her access.  Each party is free to construct their own logging forwarder around the channel they have received.  Alice will hold Bob responsible for whatever Carol does.

### Sealing and unsealing

     1 contract MakeSealerUnsealer(ret) =  {
     2     new (sealerIn, sealerOut):iopair,
     3         (unsealerIn, unsealerOut):iopair,
     4         mapRet in {
     5 
     6         ret!(*sealerOut, *unsealerOut) |
     7         MakeCoatCheck(mapRet) |
     8         for (cc <- mapRet) {
     9             for (@value, ret <= sealerIn) {
    10                 cc!("new", *ret, value)
    11             } |
    12             for (ticket, ret <= unsealerIn) {
    13                 cc!("get", *ret, *ticket)
    14             }
    15         }
    16     }
    17 }

A sealer/unsealer pair gives the same functionality as public keys, but without cryptography. It's merely an attenuation of the coat check described above.  This design pattern can be used to sign something on a user's behalf.  In the Rholang blockchain tutorial, we'll see that it even works on the blockchain because there are no secrets to store, only unforgeable names to be kept inaccessible.

### Beware of sending attenuators

A basic principle to keep in mind with RChain processes is one that is similar to more traditional web applications: whatever code you send to another party can be disassembled.  Ever since the late 1990s when buying things over the web became possible, [there have been e-commerce platforms](https://blog.detectify.com/2016/11/17/7-most-common-e-commerce-security-mistakes/) where the platform relied on the users' browsers to send the correct price of the item back to it.  The authors didn't think about the user opening the developer tools and changing the price before it got sent back.  The right way to build an e-commerce platform is to store the prices on the server and check them there.

Suppose that Bob is willing to run some code for Alice; he has a contract that says something like, "Get a process from this channel and run it."

    for (@P <- x) { P }

This is just like a web browser being willing to run the JavaScript code it gets from a website.  If Alice sends Bob an attenuating forwarder, Bob can use the pattern matching productions in Rholang to take apart the process and get access to the underlying resource.  Instead, like in the e-commerce example, Alice should only send code that forwards requests to her own processes and do the attenuation there.

## Conclusion

RChain is a language designed for use on a blockchain, but we have not mentioned anything about nodes, namespaces, wallets, Rev and phlogiston, network structure, or Casper.  A forthcoming document will address all these issues and more.

We hope that the foregoing examples spark a desire to write more code and demonstrate the ease of expressing concurrent designs.