## Normalization

This will be a *somewhat* technical bit, explaining what happens to code when it gets normalized.
Code is normalized for:
- Alpha equivalence via variable renaming
- Sorting for easy matching

(Because of the matcher, not everything commutes via `|` and not everything that ought to be alpha
  equivalent actually is.)
