## Bundles
Bundles can be used to control the read/write privileges on a channel. They are most applicable and useful when several processes are communicating through the same channel. For example, you'd like for other people to write you emails, not read your emails! Also, bundles cannot be deconstructed through pattern matching.

### Write-only
The syntax for a *write-only* bundle is: `bundle+{P}`.

Processes with only a handle on `bundle+{P}` (and not `P`) can send messages to the channel `@P`, but they cannot consume data from `@P`. To send a message to `@P`, one simply does `@{bundle+{P}}!(Msg)`. However, `for(x <- @{bundle+{P}}){Cont}` returns the error: `Trying to read from non-readable channel`. The `+` means you can only add to the channel, not subtract!

A process with a handle on the underlying process `P` can, of course, read messages sent to the channel `@P`.

```rholang
  new a, out(`rho:io:stdout`) in {
    @{bundle+{*a}}!(Nil) |
    for (_ <- a) { out!("received") }
  }

// prints to stdout: "received"
```

### Read-only
The syntax for a *read-only* bundle is: `bundle-{P}`.

Processes with only a handle on `bundle-{P}` (and not `P`) can receive messages on `@P`, but they cannot send messages on `@P`. To consume a message from `@P`, one simply does `for(x <- @{bundle-{P}}){Cont}`. However, `@{bundle-{P}}!(Msg)` returns the error: `Trying to send on non-writeable channel`. The `-` means you can only subtract from the channel, not add!

A process with a handle on the underlying process `P` can, of course, write messages to the channel `@P`.

```rholang
  new a, out(`rho:io:stdout`) in {
    a!(Nil) |
    for (_ <- @{bundle-{*a}}) { out!("sent") }
  }

// prints to stdout: "sent"
```

### No-Read/No-Write
The syntax for a *no-read/no-write* bundle is: `bundle0{P}`.

Processes with only a handle on `bundle0{P}` (and not `P`) cannot read from or write to the channel `@P`. The `0` means nothing can be added or taken.

A process with a handle on the underlying process `P` can, of course, read from and write messages to the channel `@P`.

### Read/Write
The syntax for a *read/write* bundle is: `bundle{P}`.

Processes with only a handle on `bundle{P}` (and not `P`) can both read from and write to the channel `@P`. However, `bundle{P}` cannot be deconstructed through pattern matching. There is no construct for matching (any) bundles.

A process with a handle on the underlying process `P` can, of course, read from and write messages to the channel `@P` and deconstruct `P` through pattern matching.
