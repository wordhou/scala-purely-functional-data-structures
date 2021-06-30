package word.immutable

class MergeableHeapIterator[A, H <: MergeableHeap[A, H]](var heap: H) extends Iterator[A] {
  override def hasNext: Boolean = heap.nonEmpty

  override def next(): A = {
    val (value, nextHeap) = heap.firstAndRest
    heap = nextHeap
    value
  }
}
