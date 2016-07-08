package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    a <- arbitrary[Int]
    h <- oneOf(const(empty), genHeap)
  } yield insert(a, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  // If you insert any two elements into an empty heap, finding the minimum of the resulting heap should get the smallest of the two elements back.
  property("insert2") = forAll { (a1: Int, a2: Int) =>
    val h = insert(a2, insert(a1, empty))
    val min = Math.min(a1, a2)
    findMin(h) == min
  }

  // If you insert an element into an empty heap, then delete the minimum, the resulting heap should be empty.
  property("insert&Delete") = forAll { (a: Int) =>
    val h = insert(a, empty)
    val h2 = deleteMin(h)
    h2 == empty
  }

  // Finding a minimum of the melding of any two heaps should return a minimum of one or the other.
  property("meldMin") = forAll { (h1: H, h2: H) =>
    val m1 = if (isEmpty(h1)) 0 else findMin(h1)
    val m2 = if (isEmpty(h2)) 0 else findMin(h2)
    val min = Math.min(m1, m2)
    findMin(meld(h1, h2)) == min
  }

  // Given any heap, you should get a sorted sequence of elements when continually finding and deleting minimum. (Hint: recursion and helper functions are your friends.)
  property("remMin") = forAll { (h: H) =>
    def remMin(ts: H, result: List[Int]): List[Int] = {
      if(isEmpty(ts)) result
      else findMin(ts) :: (remMin(deleteMin(ts), result))
    }
    val xs = remMin(h, Nil)
    xs == xs.sorted
  }

  // Take two arbitrary heaps, meld together. Then remove min from 1 and insert into 2, meld the results. Compare two melds by comparing sequences of ranks.
  property("meldMinMove") = forAll { (h1: H, h2: H) =>
    def remMin(ts: H, result: List[Int]): List[Int] = {
      if (isEmpty(ts)) result
      else findMin(ts) :: remMin(deleteMin(ts), result)
    }
    val meld1 = meld(h1, h2)
    val min1 = findMin(h1)
    val meld2 = meld(deleteMin(h1), insert(min1, h2))
    val xs1 = remMin(meld1, Nil)
    val xs2 = remMin(meld2, Nil)
    xs1 == xs2
  }
}
