package quickcheck

import java.lang.Math

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._
import org.scalacheck.Prop.{BooleanOperators, forAll}

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = oneOf(const(empty), for {
    v <- arbitrary[Int]
    h <- genHeap
  } yield insert(v, h)
  )

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("find-min-two") = forAll { (a: Int, b: Int) =>
    val h = insert(a, insert(b, empty))
    findMin(h) == Math.min(a, b)
  }

  property("delete-min") = forAll { (a: Int) =>
    val h = insert(a, empty)
    deleteMin(h) == empty
  }

  property("delete-min-three") = forAll { (a: Int, b: Int, c: Int) =>
    val h = insert(a, insert(b, insert(c, empty)))
    deleteMin(deleteMin(h)) == insert(Math.max(a, Math.max(b, c)), empty)
  }

  property("heap-sort") = forAll { (h: H) =>

    def sort(h: H): List[Int] = {
      if (isEmpty(h)) {
        List[Int]()
      } else {
        findMin(h) :: sort(deleteMin(h))
      }
    }

    def size(h: H): Int = {
      if (isEmpty(h)) {
        0
      } else {
        size(deleteMin(h)) + 1
      }
    }

    val list = sort(h)

    size(h) == list.size

    list.sorted == list
  }

  property("find-min-with-meld") = forAll { (h1: H, h2: H) =>
    val meldedH = meld(h1, h2)

    if (isEmpty(h1) && isEmpty(h2)) {
      true
    } else if (isEmpty(h1) && !isEmpty(h2)) {
      findMin(meldedH) == findMin(h2)
    } else if (isEmpty(h2) && !isEmpty(h1)) {
      findMin(meldedH) == findMin(h1)
    } else if (!isEmpty(h2) && !isEmpty(h1)) {
      findMin(meldedH) == Math.min(findMin(h1), findMin(h2))
    } else {
      false
    }
  }
}
