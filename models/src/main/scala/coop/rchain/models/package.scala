package coop.rchain

import coop.rchain.models.ByteStringSyntax
import coop.rchain.models.ByteArraySyntax
import coop.rchain.models.StringSyntax
import coop.rchain.models.rholangn.parmanager.primitive.syntax.PrimitiveWriterSyntax

package object models {
  // Importing syntax object means using all extensions in the project
  object syntax extends AllSyntaxModels
}

// Models syntax
trait AllSyntaxModels extends ByteStringSyntax with ByteArraySyntax with StringSyntax
