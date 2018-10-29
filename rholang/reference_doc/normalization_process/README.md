## Normalization

### Alpha Equivalence
- Rename variables, using (sort of) De Bruijn indices
- Start at `x0` for every process, incl for unforgeable names
  - unforgeable names not replaced until runtime

### Canonical sort
- Sorts for the `|` to avoid issues of commutativity
  - (Can interfere with alpha equivalence.)
- Also sorts channels in joined listens
  - Can give problem if joined listening a bunch on one channel (doesn't always work in this case)
  - **you should explain the process here, maybe not go too into implications.**
- Gets rid of any `| Nil`s

### Contracts (?)
- Syntactic sugar for a single persistent send (but not sure how that affects pattern matching). << Look this up.
