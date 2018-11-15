## Sends and Receives

### Tuplespace (RSpace)
Rholang is based on the rhoVM whose computational model is centered around processes sending messages to one another to synchronize when necessary. These operations are coordinated through the tuplespace.
Although it is unique from the Turing or Von Neuman architectures that many programmers will be familiar with, it is quite understandable. All tuplespace operations can be built up from three building blocks.

![building blocks](buildingBlocks.png)

* A channel on which messages are send and received
* Data to be sent (includes [primitive types](../primatives/README.md), [data structures](../data_structures/README.md), or even entire processes. )
* Continuations which spawn new processes when a message is received

### Sends
![Sends](send.png)
Sending a process associates that process with the channel in the tuplespace. It will wait there forever until it matches with a receive on the same channel.

#### Single Send
`chan!(data)`
Sends data which must be a process on the channel chan.

#### Persistent Send
`chan!!(data)`
This send will never be consumed. While arbitrarily many receives can receive the data, it will still remain in the tuplespace.

#### Polyadic Sends
The simple sends we've described so far are sufficient to make rholang Turing complete, but it is often convenient to send multiple...

### Receives
![Receive](receive.png)

A receive associates a continuation with a channel in the tuplespace.

#### Simple Receive
(send a process) Receive a name

#### Persistent Receive

#### Contract Sugar

#### Joins

### Comm Events
![comm events lead to processes](process.png)
When data and continuation are associated with the same name in the tuplespace (and no further restriction is applied, see below) in the tuplspace, a communication event, or "comm event" happens.

#### Pattern Matching
So far we've given the impression that any send and receive on the same channel will create synchronize. But finer grained controls are possible. For starters, consider this code in which the number of polyadic data sent is different from the number expected to be received.

```rholang
new chan in {
  chan!(3, 4, 5) |

  for( x, y <- chan) {
    // Do stuff here
  }
}
```

No communication happens here because the send pattern (3 args) does not match the receive pattern (2 args). In fact much more advanced control is discussed in the chapter on [pattern matching](../pattern_matching/README.md).

## Quoting processes to make names
TODO: Explain what a _programmer_ needs to know about how names are made by quoting processes. This should be technically correct, and pragmatic information. Don't summarize the entire rho-calc paper, but probably link it.
