## Match

### Syntax
- `match Process { [MatchCases] }`
- where `MatchCases` is a nonempty, space separated list with elements of the form: `Pattern => { Process }`

Order matters. Rholang will try the patterns in order, the first one that matches evaluates and the substitutions are made into the corresponding body.

If `Process` doesn't match with any of the patterns `Pattern1` through `PatternN`, the whole term will evaluate to `Nil`.

i.e., Any `match` process of the form

  match Process {
    Pattern1 => { Process1 }
    Pattern2 => { Process2 }
    ...
    PatternN => { ProcessN }
  }

is equivalent to

  match Process {
    Pattern1 => { Process1 }
    Pattern2 => { Process2 }
    ...
    PatternN => { ProcessN }
    _        => { Nil      }
  }


For information about pattern matching, see the pattern_matching document.
