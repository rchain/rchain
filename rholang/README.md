# Rholang

Rholang is a behaviorally typed, concurrent programming language, with a focus on message-passing and formally modeled by the ρ-calculus, a reflective, higher-order extension of the π-calculus. It is designed to be used to implement protocols and "smart contracts" on a general-purpose blockchain, but could be used in other settings as well.

The language is still in the early stages of development, but for those who are interested, more information can be found in the [RChain Platform Architecture](http://rchain-architecture.readthedocs.io/en/latest/).

# Configuration

1. Clone the repository
2. Configure/fetch dependencies
    * [sbt](http://www.scala-sbt.org/0.13/docs/Installing-sbt-on-Linux.html)
    * [CUP](http://www2.cs.tum.edu/projects/cup/install.php) - can be installed using apt
    * JLex - install using apt
    * BNFC
    * Scala
4. Run `sbt bnfc:generate` to generate the parser
5. Run `sbt console` to launch the sbt console
6. In the sbt console import the compiler with `import coop.rchain.rho2rose._`
7. And then compile any Rholang ".rho" file with `Rholang2RosetteCompiler.main(Array("<path_to_Rholang_file>.rho"))`
 
Note if you make any changes you may need to run `sbt clean` or `sbt bnfc:clean`.
