## Data Structures

All computation can be encoded in the raw rho calculus just as it can be in the lambda calculus. Convincing yourself of this is an interesting and rewarding endeavor for the mathematically inclined. Rholang strives to be a useful general purpose programming language, and therefore introduces some additional native data structures.

All data structures are processes and [can be quoted] todo insert link, to create channels.

All data structures are immutable, and their methods return new data structures with the requested change (the original is not modified).


### Tuple
A tuple is an ordered collection of 1 or more processes. The constituent processes need not be of the same type.

#### Basic syntax
`(1, 2, "hello", *someChan)` A 4-tuple
`(true, false)` A 2-tuple
`(1, )` A 1-tuple (notice the trailing comma)

#### `tuple.nth`
Retrieve the nth item from the tuple or Nil if the index is out of range
Syntax: `tuple.nth(n, ack)`
Parameters:
```
n: An integer representing the index to retrieve
ack: An acknowledgement channel on which the result will be returned.
```

Examples:
```
TODO include the examples from ../../examples/tut-tuples-methods.rho
```

### List

#### List Concatenation
While not technically a method, lists support concatenation using the `++` binary operator. Both operands must be lists.

Example:
```rholang
[1, 3, 5] ++ [2, 4, 6] -> [1, 3, 5, 2, 4, 6]
```

### Set

### Map




## Methods

Each method needs its own technical description.

- `nth`
- `toByteArray`
- `hexToBytes`
- `union`
- `diff`
- `add`
- `delete`
- `contains`
- `get`
- `getOrElse`
- `set`
- `keys`
- `size`
- `length`
- `slice`

(Not yet in the interpreter)
- `confine`


Some standard form for these: function, inputs, outputs. Easy to read, uniform.
