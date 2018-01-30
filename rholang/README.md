# Rholang

Rholang is a concurrent programming language, with a focus on message-passing and formally modeled by the ρ-calculus, a reflective, higher-order extension of the π-calculus. It is designed to be used to implement protocols and "smart contracts" on a general-purpose blockchain, but could be used in other settings as well.

The language is still in the early stages of development, but for those who are interested, more information can be found in the [RChain Platform Architecture](http://rchain-architecture.readthedocs.io/en/latest/).

## Building and Running
### Building from source

1. Clone the repository
2. Configure/fetch dependencies
    * [sbt](http://www.scala-sbt.org/0.13/docs/Installing-sbt-on-Linux.html)
    * [CUP](http://www2.cs.tum.edu/projects/cup/install.php) - cannot be installed using apt.
        * Must be cup 0.11b-2014-06-11 or later. Cup does not generate a shell script on install.
        * Use something like the following (for me it lives in $HOME/.local/bin):
			```
			#! /bin/sh

			CLASSPATH="$CLASSPATH:$HOME/.local/share/java/java-cup-11b.jar"
			CLASSPATH="$CLASSPATH:$HOME/.local/share/java/java-cup-11b-runtime.jar"
			export CLASSPATH

			exec /usr/bin/java java_cup.Main "$@"
			```
    * JFlex - install using apt 
    * BNFC - must be built from [git](https://github.com/BNFC/bnfc) b0252e5f666ed67a65b6e986748eccbfe802bc17 or later
    * Scala
4. Run `sbt bnfc:generate` to generate the lexer/parser. Re-run whenever you modify the grammar
5. Run `sbt compile` to compile classes
6. Run `sbt assembly` to build a stand-alone `.jar` file

### Command-line usage

```
$ ./rho2rbl examples/token.rho
compiled examples/token.rho to examples/token.rbl
```
which is short for:
```
$ java -jar target/scala-2.11/rholang-assembly-0.1-SNAPSHOT.jar examples/token.rho 
compiled examples/token.rho to examples/token.rbl
```

### Running rbl with Rosette
After generating the rbl:
1. in ../rosette, run build.sh
2.  (cd ../rosette && ./build.out/src/rosette --boot-dir=rbl/rosette --boot=boot.rbl ../rholang/token.rbl)

## What's working, what's broken:
See [the bugtracker](https://rchain.atlassian.net/projects/RHOL/issues/RHOL-95?filter=allopenissues) for an up-to-date list of known issues.
### The bad
In general:
  * Quoting doesn't work when sending processes.
  * Similarly running a received process doesn't work.
  * Listening on channels not created by new doesn't work in general, with a few exceptions
  * Destructuring as a part of a pattern in a for or a contract doesn't work.
  * Conditional input doesn't work.
  * We currently don't support multiple-arity receives. We support multi-arity sends, and we will shortly support arity-matching. When that lands, the mismatch will cause no forward progress. Right now it causes a crash in rosette.
  * 0-arity send and receive is currently broken.
  * Whether a variable holds a name or a value is muddled. In order to do arithmetic, you must use the name directly, not `*name`. See the examples. As we implement destructuring in inputs, this will be straightened out.
### The good
Several working examples have been included in the examples/ directory, and the tests in tests/ also work. If you run into something that doesn't work, check the bugtracker to see if it's a known issue, and if not, feel free to file a bug. We want rholang to be a useful programming environment.

## Compiler Internals

### Adding a new construct to Rholang
1. Add the syntax of the new construct in `rholang/src/main/bnfc/rholang.cf`
2. Run `sbt bnfc:generate` to get the new construct into the parser
3. Add a new visit method in `src/main/scala/rholang/rosette/Roselang.scala` and specify what the new construct should translate to in RBL. For example, if we are adding the Map type to the compiler and it has type QMap in the AST and we want it to translate to a RblTable in Rosette, we would write as follows:
```
  override def visit( p : QMap, arg : A) : R = {
    Tag( s"""(new RblTable)""")
  }
```
4. Run `sbt compile` and `sbt assembly` to have that translation included into the compiler

### Details of the Compiler Source
The file `src/main/scala/rholang/rosette/Roselang.scala` is responsible for the meat of the compiler: essentially it translates the parsed Rholang AST into RBL source. The translation follows a Visitor pattern that comes with the default BNFC generator. There are a few useful implicits defined to reduce noise:
  * Productions which do not bind new variables will be implicitly paired with the empty binding environment on return.
  * A single RBL-token is either a `Tag` or a `Var`. This is implicitly converted to a `StrTermCtxt`.
	  * If the `Tag` or `Var` needs to go into a list, it must be wrapped in an explicit call to `Leaf()`. This is because scala will not apply the implicit to infer a useful type for the list, instead it will infer a useless type for the list and fail to compile.
