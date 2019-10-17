package coop.rchain.casper.util.rholang

import shapeless.labelled.FieldType

package object costacc {
  type ->>[A, B] = FieldType[A, B]
}
