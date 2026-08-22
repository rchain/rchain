## New construct

### Syntax
```
       New ::= "new" NameDecs "in" "{" Proc "}"
   NameDec ::= NameVar | NameVar "(" Uri ")"
  NameDecs ::= NameDec | NameDec "," NameDecs
```
where `NameVar` is a `Name` variable and `Uri` a uri in backticks.

The `new` construct serves as a variable binder with scope `Proc` which produces an unforgeable process for each uniquely declared variable and substitutes these (quoted) processes for the variables. Since these are technically *name* variables, the substitution mapping is between the declared variables and their corresponding *quoted* unforgeable process.

New allows for communication on private channels because of the unforgeable nature of the processes underlying the declared names. This allows for concurrent computation without issues of shared memory possible. Every process has access to messages sent on public channels which makes them rather useless. In fact, no sends/listens on a public (forgeable) channel are even committed to the tuplespace.

The only way to transfer access to an unforgeable process outside of the lexicographical scope is to send the unforgeable process on a channel which can be accessed outside the scope. E.g.

```rholang
  new a in {
    new b in {
      a!(*b)
    } |
    for (x <- a) {
      // receive the unforgeable name b outside it's lexicographical scope
      ...
    }
  }
```

There is no other way to get a handle on an unforgeable name.
