# Rholang

Rholang is a behaviorally typed, concurrent programming language, with a focus on message-passing and formally modeled by the ρ-calculus, a reflective, higher-order extension of the π-calculus. It is designed to be used to implement protocols and "smart contracts" on a general-purpose blockchain, but could be used in other settings as well.

The language is still in the early stages of development, but for those who are interested, more information can be found in the [RChain Platform Architecture](http://rchain-architecture.readthedocs.io/en/latest/).

## Building from source

1. Clone the repository
2. Configure/fetch dependencies
    * [sbt](http://www.scala-sbt.org/0.13/docs/Installing-sbt-on-Linux.html)
    * [CUP](http://www2.cs.tum.edu/projects/cup/install.php) - can be installed using apt
    * JLex - install using apt
    * BNFC - install 2.8.2 manually or just do the hackier `cd rholang; ./install.sh;`
    * Scala
4. Run `sbt bnfc:generate` to generate the parser
5. Run `sbt compile` to compile classes
6. Run `sbt assembly` to build a stand-alone `.jar` file

## Command-line usage

```
$ ./rho2rbl examples/token.rho
compiled examples/token.rho to examples/token.rbl
```
which is short for:
```
$ java -jar target/scala-2.11/rholang-assembly-0.1-SNAPSHOT.jar examples/token.rho 
compiled examples/token.rho to examples/token.rbl
```

## SBT Console
After generating the parser:
1. Run `sbt console` to launch the sbt console
2. In the sbt console import the compiler with `import coop.rchain.rho2rose._`
3. And then compile any Rholang ".rho" file with `Rholang2RosetteCompiler.main(Array("<path_to_Rholang_file>.rho"))`

Note if you make any changes you may need to run sbt clean or sbt bnfc:clean.

# Structure of rholang/ directory
```
.
├── examples                            # Contains all example contracts
│   ├── old                             # Contains all old example contracts
│   ├── hello_world_again.rho
│   ├── log_time.rho
│   └── token.rho
├── lib                                 # Any jars we use in the compiler
│   ├── java-cup-11b.jar
│   ├── java-cup-11b-runtime.jar
│   └── JLex.jar
├── project                             
│   ├── target
│   ├── BNFC.scala
│   └── build.properties
├── src                                 # All source files
│   └── main
│       ├── bnfc                        # Folder containing current BNFC spec
│       ├── bnfc_old
│       ├── java
│       │   ├── JLex
│       │   └── rholang                 # BNFC generated AST
│       │       └── parsing
│       │           ├── delimc
│       │           │   └── Absyn
│       │           ├── lambda
│       │           │   └── Absyn
│       │           ├── rholang1
│       │           │   └── Absyn
│       │           └── rholang2
│       │               └── Absyn
│       ├── k
│       └── scala
│           ├── lib                     # Helper classes for the compiler
│           │   ├── term
│           │   └── zipper
│           └── rholang
│               └── rosette             # Scala files for the compiler
├── target                              # SBT compile output
│   ├── resolution-cache
│   ├── scala-2.11
│   └── streams
├── build.sbt                           # SBT build file
├── LICENSE
└── README.md
```

## Adding a new construct to Rholang
1. Add the syntax of the new construct in `rholang/src/main/bnfc/rholang.fc`
2. Run `sbt bnfc:generate` to get the new construct into the parser
3. Add a new visit method in `src/main/scala/rholang/rosette/Roselang.scala` and specify what the new construct should translate to in RBL. For example, if we are adding the Map type to the compiler and it has type QMap in the AST and we want it to translate to a RblTable in Rosette, we would write as follows:
```
  override def visit( p : QMap, arg : A) : R = {
    combine(
      arg,
      L(G( s"""(new RblTable)"""), Top())
    )
  }
```
4. Run `sbt compile` and `sbt assembly` to have that translation included into the compiler

## Details of the Compiler Source
The file `src/main/scala/rholang/rosette/Roselang.scala` is responsible for the meat of the compiler: essentially it translates the parsed Rholang AST into RBL source. The translation follows a modified version of the FoldVisitor pattern that comes with the default BNFC generator. The main compilation action is documented in the source as follows:
```
  /* The signature of the basic compilation action is 
   * 
   *      def visit[T]( p : T, arg : A ) : R
   * 
   * Where T is the type of expression being compiled and arg is the
   * context into which the result of the compilation will be placed.
   * For example, the compilation of the Nil process is the rosette
   * expression #niv (no intrinsic value). This expression will be
   * placed into the context represented by arg.
   * 
   * The compilation process is made completely regular
   * by certain embeddings. This means we have a single data type for
   * both the context, A and the result R. This regularity ends up
   * being forced by how the visitor pattern works together with
   * certain coherence requirements on all the bodies of the visit
   * method definitions. The embeddings are as follows: 
   * 
   *   1. every expression e lifts to a tree, * t = unit( e ),
   *      which is just e regarded as a tree; 
   *   2. every tree t lifts to a location, l = L( t, T() );
   *   3. every context c can be lifted to a location l = L( V( "*H*" ), c )
   *   4. the composition of context with tree can be uniquely lifted
   *      to a composition of locations of the form
   *      l1 = L( V( "*H*" ), c )
   *      l2 = L( t, T() )
   *      (See the combine method above.)
   * 
   *  So every visit body will be of the form:
   * 
   *     combine( 
   *       arg,
   *       ( context( p ) /: p.parts )( 
   *          { 
   *             ( acc, e ) => {
   *                combine( acc, visit( e, L( V( "*H*" ), T() ) ) ) 
   *             }
   *          }
   *       )
   *     )
   * 
   *  where p.parts stands in for accessing the components of the
   *  expression p. 
   * 
   *  This folds over the parts accumulating the results of compiling
   *  the sub-expressions of p, and then placing them into the right
   *  piece of the compilation p, and then finally places the result
   *  of the fold into the context supplied by arg. Of course, p is
   *  not generally a collection of its parts and so the access of
   *  of the parts of p will be specific to the structure of the
   *  expression, p. Likewise, the combination of the results of the
   *  compilation of the components of p will be more specific than a
   *  fold. However, this gives the general intuition behind how this
   *  algorithm works. Furthermore, it works generally for any CFL.
   * 
   *  This method favors regularity and ease of reasoning over
   *  efficiency. However, it is vastly more efficient than the
   *  parser combinators method provided out of the box by Scala as
   *  testing on a parser for prolog revealed in production.
```

## Intellij Debugger Steps
1. Open the `rchain/rholang` project in Intellij
2. Goto run -> debug -> edit configurations...
3. Click on the `+` on the top left and create a new Scala Console configuration
4. Add a debugging breakpoint on any line in the source code
5. Run the compiler through the scala console as instructed above
