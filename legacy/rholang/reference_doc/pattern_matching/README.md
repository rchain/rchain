## Patterns

- A patterns is more general than a process
- A process can be thought of as a pattern that only matches to one thing
- Explains syntax fully

### Collection patterns
- List pattern
  - `Hd`, `Tl`
  - Examples
- Set pattern
  - `Hd`, `Tl`
  - Examples

### Simple types
- Within a pattern one can include one of: `Bool`, `Int`, `String`, `List`,
`Tuple`, `Map`, `Uri`, `ByteArray`.
- If the corresponding process matches in *type* then that part of the pattern does match.
- Nothing is bound here.

### `~` Pattern
- The pattern says "anything but `given_pattern`".
- No variables are bound here (you can't depend on any specific pattern)

### Logical `\/` and `/\`
- You can only conjoin *processes*. Because names are just quoted processes, you can always do this for a name, just put within the quote.
- The `/\` (logical AND) *can* bind variables
- The `\/` (logical OR) can *not* bind variables
- Example pattern: `for( @{ {17 \/ 18} /\ x } <- AGE ){ BODY[x] }` requires that the age sent over `AGE` is either `17` or `18` and then binds the age received to `x`.

### A name pattern
- Just a quoted process pattern `@{Process Pattern}` or a name variable
- Can include anything that's valid for a process pattern.
- Example:

### A process is a pattern that matches only one process
- This means there are no (globally) free variables, e.g. in `new age in { age!(x) }`, where `x` is free.
- No collection patterns, simple types, pattern negations, logical connectives, wildcards, etc. outside of self-contained patterns.
- If it were to be used as a pattern it would only match to exactly that process.

### Process equivalence
Patterns match up to process equivalence, which is:
- Alpha equivalence (up to renaming variables)
- Commutativity around `|` (i.e. `x | y == y | x`)
- Any extra `Nil`s (`x | Nil == x`)

**All of these are resolved in normalization.**


## Pattern Matching
- The processes that employ pattern matching are the same ones that bind variables.
  - `for(){}`, `match`, `contract`
- These bind the free variables in the body and substitute via the corresponding variables in the pattern.

## Matching with `|`
A pattern `x | y` will match with `Proc1 | Proc2 | ... | ProcN`, always in the same way:
`x` will match greedily with all the parallel processes, i.e. with all of
`Proc1 | Proc2 | ... | ProcN`, and `y` will match with `Nil`. In general, `x1 | ... | xN` will match
with `Proc1| ... | ProcM` where `x1` matches to all of `Proc1| ... | ProcM` and `x2, ... , xN` each
match with `Nil`.

There is a canonical sort in the normalization process that makes sure the order of `Proc1` through
`ProcN` is always the same, no matter what order they're written in the program, as long as the
variables `x1` through `xM` don't change order. **As it stands, the normalizer does alpha
equivalence up to ordering of the free variables in the pattern.**

## A Type Theoretic Approach
One can derive a structural type system from the abstract syntax trees of processes, patterns, and names. Each process, pattern, and name gets a type which is written as its AST. We can define a type inclusion predicate, which says `type1 isIn type2` iff `type2` can be made into `type1` by appending a process or name on a leaf with a proc/name variable; appending something with type matching the simple type of a leaf; etc. with collection patterns and logical ands/ors.

This way, we can make a hierarchy of patterns. In terms of pattern-matching, this definition says that a process `Proc` matches a pattern `Pat` iff `type(Proc) isIn type(Pat)`.
