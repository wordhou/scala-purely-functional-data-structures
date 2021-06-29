package immutable

import org.scalatest.flatspec.AnyFlatSpec

class LeftistHeapTest extends AnyFlatSpec {
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
  ).map(LeftistHeap.empty[Int] :++ _)

  "An empty heap" should "be empty" in {
    assert(empty.isEmpty)
  }

  it should "have size 0" in {
    assert(empty.size == 0)
  }

  it should "return a None when findAndDeleteMin is invoked" in {
    assert(empty.findAndDeleteMin().isEmpty)
  }

  it should "return a None when findMin is invoked" in {
    assert(empty.findMin().isEmpty)
  }

  it should "return a None when deleteMin is invoked" in {
    assert(empty.deleteMin().isEmpty)
  }

  "A binomial heap (with one element)" should "be non-empty" in {
    assert(one.nonEmpty)
  }

  it should "have size 1" in {
    assert(one.size == 1)
  }

  it should "give its only element and an empty heap when findAndDeleteMin is invoked" in {
    assert(one.findAndDeleteMin().contains((11, empty)))
  }

  it should "give an empty LeftistHeap when deleteMin is invoked" in {
    assert(one.deleteMin().contains(empty))
  }

  it should "give its value when findMin is invoked" in {
    assert(one.findMin().contains(11))
  }

  "A binomial heap (with two elements)" should "be non-empty in" in {
    twos.foreach(heap => assert(heap.nonEmpty))
  }

  it should "give the lesser value when findMin is invoked" in {
    twos.foreach(heap => assert(
      heap.findMin().contains(11)
    ))
  }

  "A binomial heap (with many elements)" should "remove elements in increasing order when iterated" in {
    many.foreach(heap =>
      assert(Seq.from(heap) == Seq.range(1, 11))
    )
  }

}
