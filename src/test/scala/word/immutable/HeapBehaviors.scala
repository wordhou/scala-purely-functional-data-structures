package word.immutable

import org.scalatest.flatspec.AnyFlatSpec

trait HeapBehaviors {
  this: AnyFlatSpec =>
  def emptyHeap[H <: Heap[Int]](empty: MergeableHeap[Int, H]): Unit = {
    "An empty heap" should "be empty" in {
      assert(empty.isEmpty)
    }

    it should "have size 0" in {
      assert(empty.size == 0)
    }

    it should "return a None when findAndDeleteMin is invoked" in {
      assert(empty.firstAndRestOption.isEmpty)
    }

    it should "return a None when findMin is invoked" in {
      assert(empty.firstOption.isEmpty)
    }

    it should "return a None when deleteMin is invoked" in {
      assert(empty.restOption.isEmpty)
    }
  }

  def singletonHeap[A, H <: Heap[A]](empty: MergeableHeap[A, H], one: MergeableHeap[A, H], value: A): Unit = {
    "A binomial heap (with one element)" should "be non-empty" in {
      assert(one.nonEmpty)
    }

    it should "have size 1" in {
      assert(one.size == 1)
    }

    it should "give its only element and an empty heap when findAndDeleteMin is invoked" in {
      assert(one.firstAndRestOption.contains((value, empty)))
    }

    it should "give an empty BinomialHeap when deleteMin is invoked" in {
      assert(one.restOption.contains(empty))
    }

    it should "give its value when findMin is invoked" in {
      assert(one.firstOption.contains(value))
    }
  }

  def heapsWithTwoElements[A, H <: Heap[A]](twos: Iterable[MergeableHeap[A, H]], lesser: A, greater: A): Unit = {
    "A binomial heap (with two elements)" should "be non-empty in" in {
      twos.foreach(heap => assert(heap.nonEmpty))
    }

    it should "give the lesser value when findMin is invoked" in {
      twos.foreach(heap => assert(
        heap.firstOption.contains(lesser)
      ))
    }
  }

  def heapsWithManyElements[A, H <: Heap[A]](many: Iterable[MergeableHeap[A, H]], sorted: Seq[A]): Unit = {
    "A binomial heap (with many elements)" should "remove elements in increasing order when iterated" in {
      many.foreach(heap =>
        assert(Seq.from(heap) == sorted)
      )
    }
  }
}
