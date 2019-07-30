package coop.rchain.casper.genesis.contracts

import scala.io.Source

abstract class GeneratedSource(path: String) {

  val optionalSubstitutions: Option[Seq[(String, String)]]

  private val preSubSource: String = Source.fromResource(path).mkString

  lazy val sourceCode: String = optionalSubstitutions match {
    case Some(substitutions) =>
      substitutions.foldLeft(preSubSource) {
        case (acc, (name, value)) => acc.replace(s"""$$$$$name$$$$""", value)
      }
    case None => preSubSource
  }
}
