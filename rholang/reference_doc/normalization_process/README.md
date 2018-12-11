## Normalization

### Alpha Equivalence
- Rename variables, using (sort of) De Bruijn indices
- Start at `x0` for every process, incl for unforgeable names
  - unforgeable names not replaced until runtime
- The normalizer achieves alpha equivalence up to ordering of (locally) variables in the patterns.

### Canonical sort along `|`
- Sorts for the `|` to avoid issues of commutativity
  - (Can interfere with alpha equivalence.)
- Also sorts channels in joined listens
  - Can give problem if joined listening a bunch on one channel (doesn't always work in this case)
- Gets rid of any `| Nil`s

### Top level expressions
- Evaluated each time a process is spawned.
- Top level includes anything not in a pattern or in the body of a pattern-matching (var binding)
process, i.e. `for`, `match`, or `contract`.
- For example, `@{5+7}!(Nil)` and `for( x <- @{12} ){ ... }` will form a comm event.

### Contracts
- Syntactic sugar for a single persistent send. So, for example, the following code evaluates to
`@Nil!("success")`

  match { for(@Nil <= @Nil){Nil} } {
    {contract @Nil (@Nil) = { Nil }} => { @Nil!("success") }
  }
- In other words, it behaves like an alias
