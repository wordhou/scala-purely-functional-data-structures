package word.immutable

import org.scalatest.flatspec.AnyFlatSpec

class LeftistHeapTest extends AnyFlatSpec with HeapBehaviors {
  val empty: LeftistHeap[Int] = LeftistHeap.empty[Int]
  val one: LeftistHeap[Int] = LeftistHeap.empty[Int] + 11

  val twos: Seq[LeftistHeap[Int]] = Seq(
    LeftistHeap.empty[Int] + 11 + 15,
    LeftistHeap.empty[Int] + 15 + 11
  )

  val many: Seq[LeftistHeap[Int]] = Seq(
    Seq(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
    Seq(10, 9, 8, 7, 6, 5, 4, 3, 2, 1),
    Seq(5, 2, 1, 4, 9, 10, 3, 6, 8, 7),
    Seq(2, 1, 4, 3, 8, 7, 10, 9, 6, 5),
  ).map(LeftistHeap.from(_))

  "An empty heap" should behave like emptyHeap(empty)

  "A binomial heap (with one element)" should behave like singletonHeap(empty, one, 11)

  "A binomial heap (with two elements)" should behave like heapsWithTwoElements(twos, 11, 15)

  "A binomial heap (with many elements)" should behave like heapsWithManyElements(many, Seq.range(1, 11))

}
