# Scala Code Organization
## Lib
Within the lib are the zipper and term libraries. These libraries are used
solely for producing RBL, and not for any of the Rholang parsing/compilation.
### Zipper
The zipper library is basically straight out of the (original zipper
paper)[https://www.st.cs.uni-saarland.de/edu/seminare/2005/advanced-fp/docs/huet-zipper.pdf]
### Term
Specializations of the zipper for the Roselang compiler. The only function of
note is TermZipperComposition.compose in Rewrite.scala, which composes two
contexts. Since a context is a data
structure with a hole, Composing two contexts proceeds by placing the inner
context in the hole of the outer context, leaving a larger context with a single
hole.
## Rholang / Rosette
This is where the main compiler lives.
Most of the compiler is a visitor for the AST that produces RBL directly.
An important part of the compilation logic lives in combine(), which I don't
currently understand.
