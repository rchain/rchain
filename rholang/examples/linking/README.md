# Reusing Code Through "Linking"
### Warning: The extension of Rholang to allow for importing/exporting macros is only temporary! In a future release the Rholang compiler will include a proper package management system.

## Usage
```
$ scala link.scala <libraryDirectory> <rholangSource> [<rholangSource> ...]
```
Where `<rholangSource>` is a file to be linked with packages (i.e. imports resolved) and `<libraryDirectory>` is a directory with all the Rholang sources with `export` declarations.

`link.scala` provides facility for "linking" Rholang source code. Linking is done  by trans-piling extended Rholang source into standard Rholang source. The extended Rholang includes two new keywords: `export` and `import`. These two keywords work very similarly to the `new` keyword in standard Rholang, but `export` has the restriction that only a single  name can be declared (i.e. `export x, y in { ... }` would be INVALID). Also note that `export` and `import` declarations can only appear at the "top level" of a file -- i.e. NOT inside `contract` definitions, bodies of `for` statements or `match` cases, etc. `export`s can use `import`s from other packages.

When `link.scala` is used on a Rholang source containing the `import` keyword, the import is mapped into a standard `new` statement, but with the code block following the `in` extended to include the code from the corresponding `export` declaration (which can, and should, reside in a separate file). This "linked" output can then be compiled and run as usual because it is standard Rholang.

### Example: 
Let's say X.rho contains
```
  export X in { contract X(input, return) = { return( 2 * input ) } }
```
and that Y.rho contains
```
  import X in { new Y in { contract Y(return) = { X(5, return) } } }
```
Then linking Y.rho would result in the file Y.rho.linked with the contents
```
   new X in {
     contract X(input, return) = { return( 2 * input ) } | 
     new Y in { contract Y(return) = { X(5, return) } }
   }
```

### Note
The `v0.1` and `v.02` folders contain example packages as well as tests verifying their behaviour and showing their usage. The folder name indicates the Rholang syntax version that is used in the examples.
