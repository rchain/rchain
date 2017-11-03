# Rholang

Rholang is a behaviorally typed, concurrent programming language, with a focus on message-passing and formally modeled by the ρ-calculus, a reflective, higher-order extension of the π-calculus. It is designed to be used to implement protocols and "smart contracts" on a general-purpose blockchain, but could be used in other settings as well.

The language is still in the early stages of development, but for those who are interested, more information can be found in the [RChain Platform Architecture](http://rchain-architecture.readthedocs.io/en/latest/).

## Building from Source

1. Clone the repository
2. Configure/fetch dependencies
    * [sbt](http://www.scala-sbt.org/0.13/docs/Installing-sbt-on-Linux.html)
    * [BNFC](http://bnfc.digitalgrammars.com/) v2.8.1 Nov 2016 - e.g. `cabal install bnfc`
3. Run `sbt bnfc:generate` to generate the parser
4. Run `sbt compile` to compile classes
5. Run `sbt assembly` to build a stand-alone .jar file

## Command-line usage

    $ java -jar target/scala-2.11/rholang-assembly-0.1-SNAPSHOT.jar examples/token.rho 
    compiled examples/token.rho to examples/token.rbl

## SBT Console

After generating the parser:

1. Run `sbt console` to launch the sbt console
2. In the sbt console import the compiler with `import coop.rchain.rho2rose._`
3. And then compile any Rholang ".rho" file with `Rholang2RosetteCompiler.main(Array("<path_to_Rholang_file>.rho"))`
 
Note if you make any changes you may need to run `sbt clean` or `sbt bnfc:clean`.
