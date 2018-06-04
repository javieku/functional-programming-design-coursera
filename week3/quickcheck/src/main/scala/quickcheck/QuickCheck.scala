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

}
