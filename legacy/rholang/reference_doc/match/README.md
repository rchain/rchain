## Match

### Syntax
```
    Match ::= "match" Proc { MatchCases }
    MatchCase  ::= "{" ProcPat "}" "=>" Proc
    MatchCases ::= MatchCase | MatchCase MatchCases

```
The match construct checks each `MatchCase` in order, checking whether the given `Proc` pattern-matches the left-hand `ProcPat`s. The interpreter will query the patterns in order, the first successful match (potentially resulting in variable bindings which are then passed to the right-hand `Proc`) causes the execution of the corresponding right-hand `Proc` (with variables appropriately substituted).

If the input `Proc` doesn't match any of the patterns, the whole term simply evaluates to `Nil`. Thus, any `match` process of the form
```
    match Process {
      Pattern1 => { Process1 }
      ...
      PatternN => { ProcessN }
    }
```
is equivalent to
```
    match Process {
      Pattern1 => { Process1 }
      ...
      PatternN => { ProcessN }
      _        => { Nil      }
    }
```

For information about pattern matching, see the pattern_matching document.
