package coop.rchain.models

object BlockVersion {

  /**
    * Current block version
    */
  val Current = 1

  /**
    * All supported block versions by this version of RNode software.
    *  - previous versions are supported in transition period from previous to current version
    *  - version 1 is still supported to validate previous blocks correctly
    */
  val Supported = List(Current)
}
