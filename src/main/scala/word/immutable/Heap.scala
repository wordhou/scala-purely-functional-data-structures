package word.immutable

trait Heap[A] extends Iterable[A] {
  def +(value: A): Heap[A] = appended(value)

  def appended(value: A): Heap[A]

  def :++(suffix: IterableOnce[A]): Heap[A] =
    suffix.iterator.foldLeft(this)((heap, value) => heap appended value)

  def firstAndRestOption: Option[(A, Heap[A])]

  def firstAndRest: (A, Heap[A]) = firstAndRestOption.get

  def first: A = firstOption.get

  def firstOption: Option[A] = firstAndRestOption.map(_._1)

  def rest: Heap[A] = restOption.get

  def restOption: Option[Heap[A]] = firstAndRestOption.map(_._2)
}

trait MergeableHeap[A, H <: Heap[A]] extends Heap[A] {
  def ++(other: H): H = merge(other)

  //override def :++(suffix: IterableOnce[A]): H = suffix.iterator.foldLeft(this)((heap, value) => heap appended value)
  def merge(other: H): H

  override def +(value: A): H = appended(value)

  def appended(value: A): H

  def firstAndRestOption: Option[(A, H)]

  override def firstAndRest: (A, H) = firstAndRestOption.get

  override def rest: H = restOption.get

  override def restOption: Option[H] = firstAndRestOption.map(_._2)
}

trait HeapFactory[E, H <: MergeableHeap[E, H]] {
  def empty: H

  def singleton(value: E): H

  def from(values: IterableOnce[E]): H

  def newBuilder: scala.collection.mutable.Builder[E, H]
}