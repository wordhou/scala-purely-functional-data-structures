package word.immutable

import word.immutable.BinomialHeap.Node

import scala.annotation.tailrec

case class BinomialHeap[T](nodes: List[Node[T]] = Nil)(implicit ordering: Ordering[T])
  extends MergeableHeap[T, BinomialHeap[T]] {

  def appended(value: T): BinomialHeap[T] = BinomialHeap(BinomialHeap.insertTree(Node(value), nodes))

  def merge(other: BinomialHeap[T]): BinomialHeap[T] = BinomialHeap(BinomialHeap.merge(nodes, other.nodes))

  override def size: Int = nodes.map(_.size).sum

  override def isEmpty: Boolean = nodes.isEmpty

  def firstAndRestOption: Option[(T, BinomialHeap[T])] =
    BinomialHeap.removeMinTreeOptional(nodes).map {
      case (t, ts) => (t.value, BinomialHeap(BinomialHeap.merge(t.nodes.reverse, ts)))
    }

  def iterator: MergeableHeapIterator[T, BinomialHeap[T]] = new MergeableHeapIterator[T, BinomialHeap[T]](this)
}

object BinomialHeap {
  /**
   * Creates a binomial heap from an IterableOnce
   *
   * @param ordering The ordering by which the heap orders its members
   * @tparam A The type of elements contained in the heap
   * @return An empty binomial heap
   */
  def from[A](iterableOnce: IterableOnce[A])(implicit ordering: Ordering[A]): BinomialHeap[A] =
    iterableOnce.iterator.foldLeft(empty[A])((heap, value) => heap + value)

  /**
   * Creates an empty binomial heap.
   *
   * @param ordering The ordering by which the heap orders its members
   * @tparam A The type of elements contained in the heap
   * @return An empty binomial heap
   */
  def empty[A](implicit ordering: Ordering[A]): BinomialHeap[A] = BinomialHeap()

  /**
   * A multi-way tree node where every node contains a value, and the shape satisfies the Binomial tree property.
   * A multi-way tree of rank `n` satisfies the binomial tree property if each of its children is also a binomial tree
   * descending ranks from `n - 1, ..., 0`.
   *
   * @param value The value associated with the node
   * @param rank  The number of children of the node
   * @param nodes The children of the node
   * @tparam A The type of element contained in the tree
   */
  protected case class Node[A](value: A, rank: Int = 0, nodes: List[Node[A]] = Nil) {
    /**
     * Creates a new node with another node added to the children of this node.
     * @param other The node to add
     * @return A copy of this node with the other node added to the children of this node
     */
    def add(other: Node[A]): Node[A] = copy(nodes = other :: nodes, rank = rank + 1)

    /**
     * Finds the number of values contained in this node.
     * @return The number of values that this node contains, assuming this node is a Binomial tree.
     */
    def size: Int = 1 << rank
  }

  protected object Node {
    def link[T](a: Node[T], b: Node[T])(implicit ordering: Ordering[T]): Node[T] = {
      require(a.rank == b.rank)
      if (ordering.compare(a.value, b.value) <= 0) a add b
      else b add a
    }
  }

  type Nodes[A] = List[Node[A]]

  /**
   * Inserts a tree into a list of trees, linking trees when necessary. Assumes that `node.rank <= nodes.head.rank`.
   * @param node The tree to insert
   * @param nodes The list of trees
   * @param ordering The ordering by which the heap orders its members
   * @tparam A The type of elements in the heap
   * @return A new list of trees containing all of the elements from the added trees
   */
  @tailrec
  private def insertTree[A](node: Node[A], nodes: Nodes[A])(implicit ordering: Ordering[A]): Nodes[A] =
    (node, nodes) match {
      case (tr, Nil) => tr :: Nil
      case (tr, t :: ts) => if (tr.rank < t.rank) tr :: nodes else insertTree(Node.link(tr, t), ts)
    }

  /**
   * Merges two lists of trees into one list of trees while maintaining the property that the trees are in increasing
   * order by rank, linking trees as necessary.
   * @param nodesA The first list of trees to merge
   * @param nodesB The second list of trees to merge
   * @param ordering The ordering by which the heap orders its members
   * @tparam T The type of elements in the heap
   * @return A new list of trees containing all of the elements from the input lists of nodes
   */
  private def merge[T](nodesA: Nodes[T], nodesB: Nodes[T])(implicit ordering: Ordering[T]): Nodes[T] =
    (nodesA, nodesB) match {
      case (as, Nil) => as
      case (Nil, bs) => bs
      case (a :: as, b :: bs) if a.rank < b.rank => a :: merge(as, b :: bs)
      case (a :: as, b :: bs) if a.rank > b.rank => b :: merge(a :: as, bs)
      case (a :: as, b :: bs) => insertTree(Node.link(a, b), merge(as, bs))
    }

  /**
   * Finds the tree with the minimum root value from a list of trees, returning that tree and the original list of trees
   * with the minimum tree removed. If the list of trees contains no values, throws an Exception.
   * @param nodes The list of trees to search
   * @param ordering The ordering by which the heap orders its members
   * @tparam T The type of elements in the heap
   * @return A tuple containing the tree with the minimum root value, and the list of trees with the minimum tree removed
   */
  private def removeMinTree[T](nodes: Nodes[T])(implicit ordering: Ordering[T]): (Node[T], Nodes[T]) = {
    nodes match {
      case t :: Nil => (t, Nil)
      case t :: ts => val (tt, tts) = removeMinTree(ts)
        if (ordering.compare(t.value, tt.value) <= 0) (t, ts) else (tt, t :: tts)
    }
  }

  /**
   * Finds the tree with the minimum root value from a list of trees, returning an Option containing that tree and the
   * original list of trees with the minimum tree removed. If the list of trees contains no values, returns a None.
   * @param nodes The list of trees to search
   * @param ordering The ordering by which the heap orders its members
   * @tparam T The type of elements in the heap
   * @return An `Some` containing a tuple containing the tree with the minimum root value, and the list of trees with
   *         the minimum tree removed, or `None` if the original list of trees is empty.
   */
  private def removeMinTreeOptional[T](nodes: Nodes[T])(implicit ordering: Ordering[T]): Option[(Node[T], Nodes[T])] =
    nodes match {
      case Nil => None
      case t :: Nil => Some((t, Nil))
      case t :: ts => val (tt, tts) = removeMinTree(ts)
        Some(
          if (ordering.compare(t.value, tt.value) <= 0) (t, ts) else (tt, t :: tts)
        )
    }
}

