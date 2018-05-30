# Rholang

Rholang is a concurrent programming language, with a focus on message-passing and formally modeled by the ρ-calculus, a reflective, higher-order extension of the π-calculus. It is designed to be used to implement protocols and "smart contracts" on a general-purpose blockchain, but could be used in other settings as well.

The language is still in the early stages of development, but for those who are interested, more information can be found in the [RChain Platform Architecture](http://rchain-architecture.readthedocs.io/en/latest/).

Currently we have a working interpreter for the language. It should be considered an early preview of the language.

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
    * BNFC - MUST be built from [git](https://github.com/BNFC/bnfc) b0252e5f666ed67a65b6e986748eccbfe802bc17 or later. If you use `cabal install` you will need to add your BNFC binary to the PATH.
    * [libsodium](https://github.com/jedisct1/libsodium) - You can use the scripts/install_sodium.sh helper script
    * Scala
4. Run `sbt rholang/bnfc:generate` to generate the lexer/parser. Re-run whenever you modify the grammar
5. Run `sbt rholang/compile` to compile classes
6. Run `sbt rholangCLI/assembly` to build a stand-alone `.jar` file

### Command-line usage

```
$ java -jar rholang-cli/target/scala-2.12/rholangCLI-assembly-0.1.0-SNAPSHOT.jar rholang/tests/mercury-tut/coat_check_test.rho
<interpreter output follows.>
```

The interpereter can also be run as a REPL. Currently it won't accept multiline input, so each line must be a fully formed term.

## What's working, what's broken:
See [the bugtracker](https://rchain.atlassian.net/projects/RHOL/issues/RHOL-95?filter=allopenissues) for an up-to-date list of known issues.
### The bad
In general:
  * Conditional input doesn't work.
  * 0-arity send and receive is currently broken.
  * Matching is incomplete. We won't currently destructure receives or matches. We will match expressions.
  * We don't pre-evaluate match cases. So matching 7 + 8 as a pattern currently doesn't work. Instead, you must match against 15.
  * There is also work to support native functions. It hasn't landed yet.
### The good
Several working examples have been included in the examples directory, and the examples in the [Rholang tutorial](https://github.com/rchain/rchain/blob/master/docs/rholang/rholangtut-0.2.md) also work. If you run into something that doesn't work, check the bugtracker to see if it's a known issue, and if not, feel free to [file a bug](https://rchain.atlassian.net/secure/CreateIssueDetails!init.jspa?pid=10105&issuetype=10103&versions=10012&components=10004&assignee=medha&summary=issue+created%20via+link). We want Rholang to be a useful programming environment.
