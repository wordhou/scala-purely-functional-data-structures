package immutable

import scala.collection.immutable.Iterable

sealed abstract class LeftistHeap[A](implicit ordering: Ordering[A]) extends Iterable[A] {
  def rank: Int

  def +(value: A): LeftistHeap[A] = appended(value)

  def appended(value: A): LeftistHeap[A] = LeftistHeap.merge(this, LeftistHeap.singleton(value))

  def ++(other: LeftistHeap[A]): LeftistHeap[A] = concat(other)

  def concat(other: LeftistHeap[A]): LeftistHeap[A] = LeftistHeap.merge(this, other)

  def :++(suffix: IterableOnce[A]): LeftistHeap[A] =
    suffix.iterator.foldLeft(this)((heap, value) => heap appended value)

  def findAndDeleteMin(): Option[(A, LeftistHeap[A])]

  def findMin(): Option[A] = findAndDeleteMin().map(_._1)

  def deleteMin(): Option[LeftistHeap[A]] = findAndDeleteMin().map(_._2)

  override def iterator: Iterator[A] = new LeftistHeapIterator[A](this)
}

object LeftistHeap {
  def empty[A](implicit ordering: Ordering[A]): Empty[A] = Empty[A]()

  def singleton[A](value: A)(implicit ordering: Ordering[A]): LeftistHeap[A] = Node(value, 1, Empty[A](), Empty[A]())

  protected def merge[T](heap1: LeftistHeap[T], heap2: LeftistHeap[T])(implicit ordering: Ordering[T]): LeftistHeap[T] =
    (heap1, heap2) match {
      case (_, Empty()) => heap1
      case (Empty(), _) => heap2
      case (Node(x, _, a1, b1), Node(y, _, _, _)) if ordering.compare(x, y) <= 0 => makeNode(x, a1, merge(b1, heap2))
      case (_, Node(y, _, a2, b2)) => makeNode(y, a2, merge(b2, heap1))
    }

  private def makeNode[T](value: T, a: LeftistHeap[T], b: LeftistHeap[T])(implicit ordering: Ordering[T]): LeftistHeap[T] = {
    if (a.rank >= b.rank) Node(value, b.rank + 1, a, b)
    else Node(value, a.rank + 1, b, a)
  }

  protected case class Empty[A]()(implicit ordering: Ordering[A]) extends LeftistHeap[A] {
    override val size: Int = 0
    override val rank: Int = 0
    override val isEmpty = true

    override def findAndDeleteMin(): Option[(A, LeftistHeap[A])] = None
  }

  protected case class Node[A](value: A,
                               rank: Int = 1,
                               left: LeftistHeap[A],
                               right: LeftistHeap[A]
                              )(implicit ordering: Ordering[A]) extends LeftistHeap[A] {
    override val isEmpty = false

    override def size: Int = 1 + left.size + right.size

    override def findAndDeleteMin(): Option[(A, LeftistHeap[A])] =
      Some((value, merge(left, right)))
  }
}

private class LeftistHeapIterator[A](var heap: LeftistHeap[A]) extends Iterator[A] {
  override def hasNext: Boolean = heap.nonEmpty

  override def next(): A = {
    val Some((head, tail)) = heap.findAndDeleteMin()
    heap = tail
    head
  }
}
