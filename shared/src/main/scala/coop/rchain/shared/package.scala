package coop.rchain

import coop.rchain.store.{KeyValueStoreSyntax, KeyValueTypedStoreSyntax}

package object shared {
  // Syntax for shared project
  // TODO: unify syntax (extensions) with catscontrib,
  //  one import per project similar as `import cats.syntax.all._`
  object syntax extends KeyValueStoreSyntax with KeyValueTypedStoreSyntax
}
