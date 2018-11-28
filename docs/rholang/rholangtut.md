# A Rholang tutorial

Rholang is a new programming language designed for use in distributed systems.  Like all newborn things, it is growing and changing rapidly; this document describes the syntax in the latest version of RNode.

Rholang is "process-oriented": all computation is done by means of message passing.  Messages are passed on "channels", which are rather like message queues; however, the channels behave more like bags (multisets) rather than queues, since there is no implicit ordering on messages.  

Rholang is completely asynchronous, in the sense that while you can read a message from a channel and then do something with it, you can't send a message and then do something once it has been received---at least, not without explicitly waiting for an acknowledgment message from the receiver.  Every channel has a name, and every name denotes a unique channel.

## Getting started

There is not an IDE for Rholang. Get started with Rholang by selecting one of the options below.
* __Run Rholang on RNode__ - Write Rholang contracts in an editor of your choice and run them on RNode using either the REPL or EVAL modes. [Get started](https://github.com/rchain/rchain/releases) with the latest version of RNode.
* __Run Rholang on a web interface__ - This [web interface](http://rchain.cloud) was created by a RChain community member.
* __Write Rholang using an IntelliJ plugin__ - This [Rholang IntelliJ plugin](https://github.com/tgrospic/rholang-idea) was created by a RChain community member.

## Summary of the language constructs
Rholang has two kinds of values: processes and names.

### Names
A name represents a communication channel. You can send messages to a name or you can receive a message from a name. 

The names are created with the construct
 
    new someName in {
    //... code using someName
    }

In the above example, the name `someName` is private. By "private", we mean that no other process can send or receive messages over this channel unless we explicitly send its name to the other process.  

If rholang is running on the blockchain, the messages sent on this channel will be publicly visible, so it is not "private" in the sense of being secret.  Channels created with `new` cannot be mentioned explicitly in rholang source code.  Even if you see the bits of a private name on the blockchain, there is no language production to turn those bits back into the name.  

We sometimes use the term "unforgeable" to describe these names when we want to emphasize the inability to construct them from bits.

Receiving messages over a name is done using the `for` construction

    for( x <- name1, y <- name2) {
    ...
    }

### Processes
In Rholang everything is a process. Values like strings, booleans or numbers are also processes. 
The processes can be aggregated using the operator '|'. Below are a few examples:

    1
    true
    1 + 1
    new myName in {...}
    someName ! ("hello")
    for( x <- someChannel) { ... }
    p | q

### Primitive values
Rholang currently supports integers, strings, booleans, tuples, lists, sets and maps.

### Expressions
Expressions are special because they are evaluated before sending the result to a channel. 

The following operators are used for building expressions: 

#### Arithmetic operators
The supported arithmetic operators are: `+`, `-`, `/`, `*`.

#### Relational operators
The supported relational operators are: `>`, `>=`, `<`, `<=', `==`, `!=`.

#### Logical operators
The supported logical operators are: `and`, `or`, `not`.
 
#### Matches expression
The `p matches q` expression is similar to  

    1 match p {
    2   q -> true
    3   _ -> false
    4 }
    
The difference between `matches` and the above is that the former is an expression.

## Sending and receiving data

    1 new HelloWorld, stdout(`rho:io:stdout`) in {
    2   HelloWorld!("Hello, world!") |
    3   for (@text <- HelloWorld) {
    4     stdout!(text)
    5   }
    6 }

1) This line declares a new name-valued variable `HelloWorld` and assigns to it a newly-created private name.  

2) Every name is of the form `@P`, where `P` is a rholang process.  The `!` production takes a name `n` on its left and a process `P` on its right, then sends `@P` over the channel named `n`.  Line 2 forms the name `@"Hello, world"` and sends it on the channel whose name is stored in the variable `HelloWorld`.

3) This `for` production creates a process that waits for a single message to be sent on the channel whose name is stored in the variable `HelloWorld`.  The pattern `@text` gets matched against the serialized process, binding the process-valued variable `text` to the original process that was sent.

4) Rholang runtime environments may choose to include built-in processes listening on channels.  In this tutorial, we use new with the urn `rho:io:stdout` to request a channel where sent messages get printed to a console.

### Name Equivalence

It is possible to write one single name in several different ways. For example, the two following channels are equivalent:

    @{10 + 2}
    @{5 + 7}

Any message sent over these channels can be received by listening on the channel `@12`. There are other instances in which a name can be written in two different ways. The guiding principle for this is that if `P` and `Q` are two equivalent processes, then `@P` and `@Q` are equivalent names. In particular, all of the following channels are equivalent:

    @{ P | Q }
    @{ Q | P }
    @{ Q | P | Nil }

Before using a channel, Rholang first evaluates expressions and accounts for these `|` rules at the top level--but only at the top level. This means that if an arithmetic expression forms part of a pattern within a pattern, it is left untouched. Because of this,

    for( @{ x + 5 } <- @"chan" ){ ... }

will never receive any message on `@"chan"` since if we send anything, such as `10 + 5`, over `@"chan"`, the arithmetic expression gets evaluated and the name `@15` is sent.

Finally, channels also respect a change in variable name (alpha equivalence), so the following channels are equivalent:

    @{ for( x <- chan ){ ... } }
    @{ for( z <- chan ){ ... } }

## Replicated receive

    1 new HelloWorld, stdout(`rho:io:stdout`) in {
    2   for (@text <= HelloWorld) {
    3     stdout!(text)
    4   } |
    5   HelloWorld!("Hello, world!") |
    6   HelloWorld!("Hola, mundo!")
    7 }

2) Instead of handling only a single message, a `for` using a double arrow `<=` will persist, spawning a copy of the body for each message received.

5-6) We send the string processes `"Hello, world!"` and `"Hola, mundo!"` on the channel `HelloWorld`.  It is non-deterministic which message will be processed first.

## Contracts as sugar for replicated receive

    1 new HelloWorld, stdout(`rho:io:stdout`) in {
    2   contract HelloWorld(@text) = {
    3     stdout!(text)
    4   } |
    5   HelloWorld!("Hello, world!") |
    6   HelloWorld!("Hola, mundo!")
    7 }

2) The only difference between this example and the last one is this line.  The `contract` production is syntactic sugar.  The process `contract Name(...formals) = { P }` means the same as `for (...formals <= Name) { P }`.

## Replicated send

    1 new HelloWorld, stdout(`rho:io:stdout`), stderr(`rho:io:stderr`) in {
    2   HelloWorld!!("Hello, world!") |
    3   for (@text <- HelloWorld) {
    4     stdout!(text)
    5   } |
    6   for (@text <- HelloWorld) {
    7     stderr!(text)
    8   }
    9 }

2) The double-bang `!!` means that this message will be sent again as soon as it is read.

3-4, 6-7) There are two listening processes; each one consumes a single message from the channel and forwards it to either `"stdout"` or `"stderr"`.  The order in which they get forwarded to those channels is nondeterministic.

## Sequential send

In order to have one message follow after another is known to have been received, we must use an acknowledgement message.

    1 new chan, ack, stdoutAck(`rho:io:stdoutAck`) in {
    2   chan!(0) |
    3   for (_ <- ack) {
    4     chan!(1)
    5   } |
    6   for (@num <= chan) {
    7     stdoutAck(num, *ack)
    8   }
    9 }

2) We send the message 0 on `chan`.
3) We wait for a message on the channel `ack`, throw it away, and then proceed with the body.
4) We send the message 1 on `chan`.
6) We listen persistently for messages sent on `chan`.
7) We forward each message to the channel `"stdoutAck"`, which expects both a value to print and a channel on which to send an acknowledgement message that the text has been received and printed.  In this program, we are guaranteed that 0 will be printed before 1.

## Sending and receiving multiple processes

     1 new chan, stdout(`rho:io:stdout`) in {
     2   chan!(1,2,3) |
     3   chan!((4,5,6)) |
     4   chan!(7,8) |
     5   chan!([9, 10], 11) |
     6   chan!(12 | 13) |
     7   for (@x, @y, @z <= chan) { 
     8     stdout!(["three", x, y, z])
     9   } |
    10   for (@a, @b <= chan) {
    11     stdout!(["two", a, b])
    12   } |
    13   for (@a <= chan) {
    14     stdout!(["one", a])
    15   }
    16 }

2) We send three numeric processes on `chan`.  This send necessarily synchronizes with the `for` on line 7.
3) We send one tuple process on `chan`.  This send necessarily synchronizes with the `for` on line 13.
4) We send two numeric processes on `chan`.    This send necessarily synchronizes with the `for` on line 10.
5) We send a list process and a numeric process on `chan`.    This send necessarily synchronizes with the `for` on line 10.
6) We send a single process that is the par of two numeric expressions on `chan`.  This send necessarily synchronizes with the `for` on line 13.

## Pattern matching and channels as databases

There are two kinds of value in Rholang, names and processes.  Patterns are names or processes with free variables and logical connectives, which appear to the left of an arrow in a `for` or a `match`:

    for (<name pattern> <- <channel>) { <process> }
    match <process> { <process pattern> => <process> }

A variable is either name-valued or process-valued.  It is an error to use a process-valued variable in name position and vice-versa.  Name positions are to the right of a `new`, to the right of an asterisk `*`, to the right and left of an arrow in a `for` (and in the desugaring of a contract), and to the left of an exclamation mark `!` in a send.  Name-valued variables are bound by a `new` production and in name positions in patterns.  In each of the following examples, `x` is a new name-valued variable whose scope is `P`:

    new x in P
    for (w, x <- y) { P }
    for (@{x!(Q)} <- y) { P }
    for (@{for(z <- x) { Q }} <- y) { P }
    contract foo(x) = { P }
    match R { x!(Q) => P }
    match R { contract x(y) = { Q } => P }

Process-valued variables are bound in process positions in patterns.  Process positions are after an at sign `@`, in the body of a send, in the body of a `for`, and in all process constructors like expressions, collections, and so on.  In each of the following examples, `P` is a new process-valued variable whose scope is `Q`.

    contract foo(@P) = { Q }
    for (@{x!(P)} <- y) { Q }
    for (@{for (@P <- z) { R }} <- y) { Q }
    for (@{ P | R } <- y) { Q }
    match R { P => Q }
    match R { [P, S, ...T] => Q }
    match R { contract foo(x) = { P } => Q }
    match R { contract foo(@S) = { x!(P + S) } => Q }

In addition to free variables we have the logical connectives "AND", written `/\`, and "OR", written `\/`. In order to match with a list of patterns, separated by `/\`, a process or name must match each pattern in the list. For example, to send a message over a channel `students` that will be received by

    for(@{ @"grade"!(x) /\ @y!(10) } <- StudentGradeLevel){ ... }

the process we send must necessarily be of the form `@"grade"!(10)`. The first pattern requires that the process be something of the form `@"grade"!(x)`, where `x` is a process variable, and the second pattern requires that `x` be `10`. So this waits for a student to register in grade 10 and then executes the body of the `for`. If we register the student in grade 10 via

    StudentGradeLevel!(@"grade"!(10))

in parallel with the `for` above, `x` will bind to `10` and `y` will bind to `"grade"`. In contrast, in order to match a list of patterns, separated by `\/`, a process or name need only match ONE pattern in the list. Because we cannot depend on a specific pattern matching, we cannot use patterns separated by `\/` to bind any variables. For instance, replacing the `/\` with an `\/` in the `for` above yields an incorrect Rholang program

    for(@{ @"grade"!(x) \/ @y!(10) } <- StudentGradeLevel){ ... }

which is incorrect because `x` and `y` are both free. Furthermore, we cannot capture `x` or `y` because we cannot use a binder to capture variables inside a pattern. We will cover this more later on, when we talk about patterns within patterns.

To correct this code, we could write something like 

    for(@{ @"grade"!(10) \/ @"grade"!(11) } <- StudentGradeLevel){ ... }

which waits for a student to register in either grade `10` or `11` before executing the body of the `for`.

We can use both `/\` and `\/` in any given pattern, such as in: 

    for(@{ 10 \/ 20 /\ x } <- @"chan"){ ... }

This program is not quite correct. In Rholang, precedence rules for logical connectives are standard, meaning that `/\` binds tighter than `\/`, so the `for` above is equivalent to

    for(@{ 10 \/ { 20 /\ x } } <- @"chan"){ ... }

which has the free variable `x`. We can make this into a correct Rholang program by shifting parentheses

    for(@{ { 10 \/ 20 } /\ x } <- @"chan"){ ... }

Finally, logical connectives need not only separate process patterns, but can be used in any component of a pattern. For example, we can simplify our code that waits for a student to register in either grade `10` or `11` and write

    for(@{ @"grade"!(10 \/ 11) } <- StudentGradeLevel){ ... }

This will match with whichever of `StudentGradeLevel!(@"grade"!(10))` and `StudentGradeLevel!(@"grade"!(11))` runs first. The same precedence rules from before apply.

### Patterns in a `for`

The term 

    for (@{x!(P)}, @{for(y <- z) { y!(Q) }} <- chan) { ... }

expects to receive two names on the channel `chan`, where the names are serializations of certain processes that must match the given patterns.  The first name should be the serialization of a single send of a single process; the name variable `x` gets bound to the channel on which the send occurs, and the process variable `P` gets bound to its payload.  The second name should be the serialization of a single receive, whose body consists of a single send of a single process.  The name variable `z` gets bound to the channel on which the receive is listening, and the process variable `Q` gets bound to the payload of the send.  The name variable `y` does not get bound, but the matcher checks to see that the same variable is used to the left of the arrow in the `for` and to the left of the exclamation mark in the send.

Patterns let us implement structured queries on data.  Suppose that we send lots of processes with a similar structure on a name `people`:

    people!(@"name"!("Joe") | @"age"!(20) | @"eyes"!("blue") | @"seq"!(0)) |
    people!(@"name"!("Julie") | @"age"!(30) | @"eyes"!("brown") | @"seq"!(0)) |
    people!(@"name"!("Jane") | @"age"!(40) | @"eyes"!("green") | @"seq"!(0)) |
    people!(@"name"!("Jack") | @"age"!(50) | @"eyes"!("grey") | @"seq"!(0))

Then we can think of the name `people` as a table in a database and query it.  A rough translation of the SQL statement `SELECT age, name FROM people WHERE age > 35` in the context of the data above would be

     1 new people, stdout(`rho:io:stdout`) in {
     2   people!(@"name"!("Joe") | @"age"!(20) | @"eyes"!("blue") | @"seq"!(0)) |
     3   people!(@"name"!("Julie") | @"age"!(30) | @"eyes"!("brown") | @"seq"!(0)) |
     4   people!(@"name"!("Jane") | @"age"!(40) | @"eyes"!("green") | @"seq"!(0)) |
     5   people!(@"name"!("Jack") | @"age"!(50) | @"eyes"!("grey") | @"seq"!(0))|
     6   for (@{@"seq"!(0) | {row /\ {@"name"!(name) | @"age"!(age) | _}}} <= people) {
     7     if (age > 35) {
     8       stdout!([name, age])
     9     } |
    10     people!(row | @"seq"!(1))
    11   }
    12 }

6) The `for` production uses a double arrow `<=`, so it reads repeatedly from the table `people`.  It uses a pattern to match the structure of the processes sent on the channel `people`.  The pattern starts with an at sign `@`, because anything to the left of an arrow in a `for` is a name, i.e. a serialized process.

The pattern continues with `@"seq"!0`.  This bit of data prevents the `for` loop from reading the same row multiple times: once a row has been read, it's put back into the database on line 10, but the sequence number gets incremented to 1.  Since the `for` only wants rows with sequence number 0, each row only gets read once.

Next in the pattern comes `row /\ ...`.  The process variable `row` gets bound to everything in the process other than `@"seq"!0`.  The operator `/\` is the logical connective "AND", which will also try to match the next pattern to the data.

Finally in the pattern is the subpattern `{@"age"!age | @"name"!name | _}`, which binds the process variables `age` and `name`.  The underscore wildcard binds to the send on `@"eyes"` and discards it.

7) This is the translation of the `WHERE` clause.

8) Here we merely print out the name and age, but we could do arbitrary processing.  We expect to see

    @{["Jane", 40]}
    @{["Jack", 50]}
    
in either order.

10) As noted above, this line updates the sequence number so that rows are not read twice.  If we have multiple readers of the database, each will be assigned a sequence number.  As soon as a row is read, it is put back into the database for the next reader; any processing is done in parallel with the write, so even though this pattern sequentializes the readers, the access is still very fast.


### Patterns in a `match`

Patterns can also be used in a `match` production.

     1 new sumProd in {
     2   contract sumProd(@arr, ret) = {
     3     new fold, sum, prod in {
     4       contract fold(@init, op, @arr, ret) = {
     5         match arr {
     6           [h ...t] => {
     7             new tailCh in {
     8               fold!(init, *op, t, *tailCh) |
     9               for (@folded <- tailCh) {
    10                 op!(h, folded, *ret)
    11               }
    12             }
    13           }
    14           [] => ret!(init)
    15         }
    16       } |
    17       contract sum(@arr, ret) = {
    18         new add in {
    19           contract add(@a, @b, ret) = {
    20             ret!(a + b)
    21           } |
    22           fold!(0, *add, arr, *ret)
    23         }
    24       } |
    25       contract prod(@arr, ret) = {
    26         new mult in {
    27           contract mult(@a, @b, ret) = {
    28             ret!(a * b)
    29           } |
    30           fold!(1, *mult, arr, *ret)
    31         }
    32       } |
    33       new sumCh, prodCh in {
    34         sum!(arr, *sumCh) |
    35         prod!(arr, *prodCh) |
    36         for (@s <- sumCh; @p <- prodCh) {
    37           ret!([s, p])
    38         }
    39       }
    40     }
    41   } |
    42   sumProd!([4,5,6], "stdout")
    43 }

This example shows how to implement a fold over a list, then uses it to compute the sum and product of a list.

4) The `fold` contract expects a process `init`, a name `op`, a process `arr`, and a name `ret`.

5) It begins by looking at the structure of the `arr` process.

6) This line begins with a process pattern `[h ...t]` that binds the process variable `h` to the head of the list and the process variable `t` to the tail of the list.  If the pattern matches `arr`---that is, if `arr` has at least one element---then the process to the right of the arrow `=>` executes.

7) A new channel is declared for sending the intermediate result on.

8-9) Line 8 is the recursive call in the fold; it provides the intermediate result channel here and listens for the intermediate result on line 9.

10) This combines the head with the intermediate result and sends the final result over the given return channel `ret`.

17) The `sum` contract expects a process `arr` and a name `ret`.

19-21) To sum up an array, we have to add together the elements, so we define a contract that takes two things, adds them together, and sends the result on the given channel.

22) The sum of an empty array is 0.

25-32) Identical to `sum` except for using multiplication and 1 instead of addition and 0.

34-35) Invoke the `sum` and `prod` contracts simultaneously.

36) Wait until both have returned and bind process variables to the results.

37) Send a two-element list back containing the results.

42) Invoke the main contract on an example list.


### Patterns With Wildcards

We can also include wildcards in patterns. The intuition for these is that they throw away whatever they match to in the pattern. This can be useful, for example, to synchronize processes by listening on a channel `ack` for an acknowledgment that one process has completed and that the body of this `for` is supposed to execute.

    for( _ <- ack ){ ... }


### Patterns with simple types

It's possible to match simple types: `Int`, `Bool`, `String`, `Uri`, and `ByteArray`

    for( @Int <- ack) { ... }

If you want to capture the value you matched, you can use the and logcial connective: `/\`

    for( @{x /\ Int} <- ack) { ... }

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

11) Concurrently with the `get` contract, we run a contract listening on `set`.

12) We block until there's a message on `valueStore`, then read it.  We throw away the message that we read.

13) We send the new value to store on `valueStore` and signal that the operation is complete.

18-36) The usage code demonstrates creating a cell, assigning the initial value 123, getting that value, setting the value to 456, then getting that value.  

Note the deep layers of callback. Rholang was designed to make concurrent computations natural to express; as a consequence, data dependencies implicit in sequencing in other languages must be made explicit.

## Iteration

In the code below, we show an example of iterating through a list.

     1 new iterate in {
     2   contract iterate(@list, process, done) = {
     3     match list {
     4       [hd, ...tl] => {
     5         new ack in {
     6           process!(hd, *ack) |
     7           for (_ <- ack) { iterate!(tl, *process, *done) }
     8         }
     9       }
    10       _ => done!(Nil)
    11     }
    12   } |
    13   new process, done in {
    14     iterate!([4,5,6], *process, *done) |
    15     contract process(@item, ack) = {
    16       /* handle processing of item */
    17       ack!(Nil)
    18     } |
    19     for (_ <- done) {
    20       /* done! */
    21       Nil
    22     }
    23   }
    24 }

3) The `match` construction allows destructuring a variable through pattern matching.

4) List patterns support matching the remainder of a list.  If `list` matches the pattern of a head/tail pair then we execute the main body of the loop.

5) We create a channel for the item handler to notify us that it's done with the current item.

6) We invoke the processor on the item and the acknowledgement channel.

7) When we receive acknowledgement, we reinvoke the iterator on the tail.

10) If the list is empty, we signal that the processing is complete.

14) We invoke the iterator.

15-18) This `contract` gets invoked for each item in the list.  On line 17, we tell the iterator that we're done with this item.

19) This `for` contains the code that should be executed when the interaction is complete.

## Coat check

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
new x, y, stdout(`rho:io:stdout`) in {
    x!(@"name"!("Joe") | @"age"!(40)) |  // (1)
        for (@r <- x) {
            @"keccak256Hash"!(r.toByteArray(), *y) // hash the program from (1)
        } |
        for (@h <- y) {
            // The h here is the hash of the rholang term we sent to the hash channel.
            // We can do anything we want with it, but we choose to just print it.
            // Rholang prints byte arrays in hexadecimal.
            stdout!(h)  // print out the keccak256 hash
        }
}
```


### Verify

1. Let's hash a rholang program and print out it in base16. In rholang:

  ```rholang
  new x, y, stdout(`rho:io:stdout`) in { 
     x!(@"name"!("Joe") | @"age"!(40)) |  // (1)
     for (@r <- x) { @"keccak256Hash"!(r.toByteArray(), *y) } |  // hash the program from (1)
     for (@h <- y) { stdout!(h) }  // print out the keccak256 hash
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
  new x, stdout(`rho:io:stdout`) in { 
    @"ed25519Verify"!("a6da46a1dc7ed715d4cd6472a736249a4d11142d160dbef9f20ae493de908c4e".hexToBytes(), "d0a909078ce8b8706a641b07a0d4fe2108064813ce42009f108f89c2a3f4864aa1a510d6dfccad3b62cd610db0bfe82bcecb08d813997fa7df14972f56017e0b".hexToBytes(),"288755c48c3951f89c5f0ffe885088dc0970fd935bc12adfdd81f81bb63d6219".hexToBytes(), *x) | 
    for (@v <- x) { stdout!(v) } 
  } 

  ```
  and we should see: 
  ```
  @{true}
  ```

  which means that our signed hash is verified.

  If we, for example, pass in a corrupted hash, changing the initial 'a' to a 'b':
  ```
  new x, stdout(`rho:io:stdout`) in { 
     @"ed25519Verify"!("b6da46a1dc7ed615d4cd6472a736249a4d11142d160dbef9f20ae493de908c4e".hexToBytes(), "d0a909078ce8b8706a641b07a0d4fe2108064813ce42009f108f89c2a3f4864aa1a510d6dfccad3b62cd610db0bfe82bcecb08d813997fa7df14972f56017e0b".hexToBytes(),"288755c48c3951f89c5f0ffe885088dc0970fd935bc12adfdd81f81bb63d6219".hexToBytes(), *x) | 
     for (@v <- x) { stdout!(v) } 
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

Rholang is a language designed for use on a blockchain, but we have not mentioned anything about nodes, namespaces, wallets, Rev and phlogiston, network structure, or Casper.  A forthcoming document will address all these issues and more.

We hope that the foregoing examples spark a desire to write more code and demonstrate the ease of expressing concurrent designs.
