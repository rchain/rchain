package coop.rchain.casper.genesis.contracts

abstract class SourceCode(preSubSource: String) {
  val optionalSubstitutions: Option[Seq[(String, String)]]
  val sourceCode: String = optionalSubstitutions match {
    case Some(substitutions) =>
      substitutions.foldLeft(preSubSource) {
        case (acc, (name, value)) => acc.replace(s"""$$$$$name$$$$""", value)
      }
    case None => preSubSource
  }
}
