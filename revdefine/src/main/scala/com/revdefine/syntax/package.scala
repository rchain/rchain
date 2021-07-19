package com.revdefine

import com.revdefine.node.store.pagination.PaginateSyntax
import com.revdefine.node.store.{MongoObservableSyntax, RevDefineStoreSyntax}

package object syntax {
  object all extends AllRevDefineSyntax
}

// Casper syntax
trait AllRevDefineSyntax extends RevDefineStoreSyntax with PaginateSyntax with MongoObservableSyntax
