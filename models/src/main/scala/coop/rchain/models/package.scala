package coop.rchain

import coop.rchain.models.ByteStringSyntax
import coop.rchain.models.ByteArraySyntax

package object models {
  // Importing syntax object means using all extensions in the project
  object syntax extends AllSyntaxModels
}

// Models syntax
trait AllSyntaxModels extends ByteStringSyntax with ByteArraySyntax
