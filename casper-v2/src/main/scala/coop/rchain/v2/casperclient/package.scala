package coop.rchain.v2
import coop.rchain.v2.casper.data.LatestMessages

package object casperclient {

  /**
   * The highest validated messages for all known senders.
   */
  type ValidationFringe[M, S] = LatestMessages[M, S]
}
