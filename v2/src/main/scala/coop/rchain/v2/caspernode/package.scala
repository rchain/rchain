package coop.rchain.v2

import coop.rchain.v2.casper.data.LatestMessages

/**
 * Node part responsible for processing Casper messages.
 */
package object caspernode {

  /**
   * Highest validated messages. Node has to have all dependencies validated to be able to validate a child.
   */
  type ValidationFringe[M, S] = LatestMessages[M, S]
}
