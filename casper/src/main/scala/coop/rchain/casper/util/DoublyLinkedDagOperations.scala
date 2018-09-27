package coop.rchain.casper.util

import coop.rchain.casper.Estimator.BlockHash

import scala.collection.immutable.{HashMap, HashSet}

trait DoublyLinkedDag[A] {
  val parentToChildAdjacencyList: Map[A, Set[A]]
  val childToParentAdjacencyList: Map[A, Set[A]]
  val dependencyFree: Set[A]
}
case class BlockDependencyDag(
    parentToChildAdjacencyList: Map[BlockHash, Set[BlockHash]],
    childToParentAdjacencyList: Map[BlockHash, Set[BlockHash]],
    dependencyFree: Set[BlockHash]
) extends DoublyLinkedDag[BlockHash]

object BlockDependencyDag {
  def empty: BlockDependencyDag =
    new BlockDependencyDag(
      HashMap.empty[BlockHash, Set[BlockHash]],
      HashMap.empty[BlockHash, Set[BlockHash]],
      HashSet.empty[BlockHash]
    )
}

object MapHelper {
  def updatedWith[A, B](map: Map[A, B], key: A)(default: B)(f: B => B): Map[A, B] = {
    val newValue = map.get(key).fold(default)(f)
    map.updated(key, newValue)
  }
}

object DoublyLinkedDagOperations {
  def add[A](dag: DoublyLinkedDag[A], parent: A, child: A): DoublyLinkedDag[A] = {
    val parentToChildAdjacencyList: Map[A, Set[A]] = dag.parentToChildAdjacencyList
    val childToParentAdjacencyList: Map[A, Set[A]] = dag.childToParentAdjacencyList
    val dependencyFree: Set[A]                     = dag.dependencyFree

    val updatedParentToChildAdjacencyList =
      MapHelper.updatedWith(parentToChildAdjacencyList, parent)(Set(child))(_ + child)
    val updatedChildToParentAdjacencyList =
      MapHelper.updatedWith(childToParentAdjacencyList, child)(Set(parent))(_ + parent)

    val postParentDependencyFree =
      if (updatedChildToParentAdjacencyList.get(parent).exists(_.nonEmpty)) {
        dependencyFree
      } else {
        dependencyFree + parent
      }

    val postChildDependencyFree = postParentDependencyFree - child

    new DoublyLinkedDag[A] {
      override val parentToChildAdjacencyList: Map[A, Set[A]] = updatedParentToChildAdjacencyList
      override val childToParentAdjacencyList: Map[A, Set[A]] = updatedChildToParentAdjacencyList
      override val dependencyFree: Set[A]                     = postChildDependencyFree
    }
  }

  // If the element doesn't exist in the dag, the dag is returned as is
  def remove[A](dag: DoublyLinkedDag[A], element: A): DoublyLinkedDag[A] = {
    val parentToChildAdjacencyList: Map[A, Set[A]] = dag.parentToChildAdjacencyList
    val childToParentAdjacencyList: Map[A, Set[A]] = dag.childToParentAdjacencyList
    assert(!childToParentAdjacencyList.contains(element))
    assert(!parentToChildAdjacencyList.values.toSet.contains(element))
    val maybeChildren = parentToChildAdjacencyList.get(element)
    val initAcc       = (childToParentAdjacencyList, Set.empty[A])
    val (updatedChildToParentAdjacencyList, newDependencyFree) = maybeChildren match {
      case Some(children) =>
        children.foldLeft(initAcc) {
          case ((childToParentAdjacencyListAcc, dependencyFreeAcc), child) =>
            val maybeParents = childToParentAdjacencyListAcc.get(child)
            maybeParents match {
              case Some(parents) =>
                val updatedParents = parents - element
                if (updatedParents.isEmpty) {
                  (childToParentAdjacencyListAcc - child, dependencyFreeAcc + child)
                } else {
                  (childToParentAdjacencyListAcc.updated(child, updatedParents), dependencyFreeAcc)
                }
              case None =>
                throw new Error(s"We should have at least $element as parent")
            }
        }
      case None => initAcc
    }
    new DoublyLinkedDag[A] {
      override val parentToChildAdjacencyList
        : Map[A, Set[A]]                                      = dag.parentToChildAdjacencyList - element
      override val childToParentAdjacencyList: Map[A, Set[A]] = updatedChildToParentAdjacencyList
      override val dependencyFree: Set[A]                     = dag.dependencyFree ++ newDependencyFree - element
    }
  }
}
