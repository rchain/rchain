## Data Structures

The rho (reflective higher-order) calculus is Turing complete so it can encode all computation; as is also true of the lambda and pi calculi. Convincing yourself of this is an interesting and rewarding endeavor for the mathematically inclined. Since Rholang is a useful *general purpose* programming language, we therefore introduce some additional native data structures.

All data structures are processes (in fact, ground processes because nothing is sent or received) so they can be quoted to create channels. Data structures have methods defined on them for augmenting information. In a functional style, the data structures are immutable, their methods returning new data structures with the requested change (the original remaining unmodified).

Rholang supports the four basic data structures: `List`, `Map`, `Set`, `Tuple`, i.e. the type
```rholang
  DataStruct ::= List | Map | Set | Tuple
```

### List
A Rholang *list* is an ordered collection of 0 or more processes. The syntax is:
```rholang
  List ::= "[" "]" | "[" Procs "]"
```
where a collection of processes `Procs` is given by
```rholang
  Procs ::= Proc | Proc "," Procs
```

- more info

#### List Concatenation
While not technically a method, lists support concatenation using the `++` binary operator. Both operands must be lists.

Example:
```rholang
  [1, 3, 5] ++ [2, 4, 6] => [1, 3, 5, 2, 4, 6]
```

### Map
A Rholang *map* is an unordered collection of 0 or more key-value pairs; both keys and values are processes. The syntax is:
```rholang
      Map ::= "{" "}" | "{" KVPairs "}"
   KVPair ::= Proc ":" Proc
  KVPairs ::= KVPair | KVPair "," KVPairs
```

The presentation of a map in Rholang is normalized by its keys. E.g. the map `{ 2:5 , 0:2 , 1:1 }` is presented as `{ 0:2 , 1:1 , 2:5 }`.

### Set
A Rholang *set* is an unordered collection of 0 or more processes. The syntax is:
```rholang
  Set ::= "Set(" ")" | "Set(" Procs ")"
```

The presentation of a set in Rholang is normalized.

### Tuple
A Rholang *tuple* is an ordered collection of 1 or more processes. The syntax is:
```rholang
 Tuple ::= "(" Proc ",)" | "(" Proc "," Procs ")"
```

#### Examples of the syntax
- `[ ]` empty list
- `[ Set(true) , @19!(23) , { 1:2 , "a":2 }.get("a") ]` a length 3 list
- `{ }` empty map
- `{ 0:x , 1:y!(0) , "abc":"a" , Nil:{for(y <- x){*y}} }` a size 4 map
- `Set( )` empty set
- `Set( for( _ <<- @0 ){ Nil } , "a" , 42 , [ *chan , 1 ] , true )` a size 5 set
- `(true , 1 , chan!(2) , [ 1 , Nil ] , "a" , { ( Nil ,):0 } )` a 6-tuple
- `(Nil ,)` a 1-tuple (notice the trailing comma)

Examples:
```
TODO include the examples from ../../examples/tut-tuples-methods.rho
```

## Methods
All method calls have the same syntactic form:
```rholang

  MethodCall ::= Data "." MethodName "(" Args ")"
        Args ::= "" | Procs
```
where `MethodName` is a valid method name token and `Args` is a valid (for the method) list of process arguments.

Methods must be called with the correct number of argument or an error is returned.

##### `confine`
(Not yet in the interpreter)

##### `toByteArray()`
Serializes process from AST. Returns an error if called with arguments.

##### `hexToBytes()`
Converts hexadecimal encoded string to byte array. Non-hex characters are disregarded. Returns an error when called with arguments.

##### `toUtf8Bytes()`
Converts string to UTF-8 byte array. Returns an error when called with arguments or on a non-string.

#### Unordered data structure methods
##### Maps
- `contains(P)`: contains key `P` predicate
- `union(map)`: union for maps
- `diff(map)`: difference of maps
- `delete(key)`: delete pair associated with given key
- `get(key)`: retrieves the value associated with the given key, else `Nil` (equivalent to `getOrElse(key, Nil)`)
- `getOrElse(key,P)`: retrieves the value associated with the given key, else `P`
- `set(key)`: set the value of the given key
- `keys()`: retrieve the set of keys
- `size()`: number of key-value pairs

##### Sets
- `contains(P)`: contains element `P` predicate
- `union(set)`: union for sets
- `diff(set)`: difference of sets
- `add(P)`: add element `P` to a set
- `delete(P)`: delete element `P` from a set
- `size()`: number of elements in set

#### Ordered data structure methods
- `length()`: the number of elements in a byte array, list, or string
- `slice(m,n)`: slice of a byte array, list, or string
- `toSet()`: generates a Set from List or Map elements

##### `nth(Int)`
Retrieves the nth process from a byte array, list, or tuple; returns error if the given index is out of range.

// TODO: add more methods
