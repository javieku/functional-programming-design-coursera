object session {

  trait Generator[+T] {
    self => // an alias for "this"
    def generate: T

    def foreach[U](f: T => U) {
      f(generate)
    }

    def map[S](f: T => S): Generator[S] = new Generator[S] {
      def generate = f(self.generate) // also can be written "Generator.this.generate" or "this.generate"
    }

    def flatMap[S](f: T => Generator[S]): Generator[S] = new Generator[S] {
      def generate = f(self.generate).generate
    }
  }

  val integers = new Generator[Int] {
    def generate = scala.util.Random.nextInt()
  }

  val booleans = integers.map(_ >= 0)

  def choose(from: Int, to: Int) = new Generator[Int] {
    def generate = if (from == to) from else scala.util.Random.nextInt(to - from) + from
  }

  def choose2(lo: Int, hi: Int): Generator[Int] =
    for (x <- integers) yield lo + x % (hi - lo) //normalize in the interval between lo and hi using this modulator function

  def oneOf[T](xs: T*): Generator[T] =
    for (idx <- choose(0, xs.length)) yield xs(idx)

  def single[T](x: T) = new Generator[T] {
    def generate = x
  }

  def pairs[T, U](t: Generator[T], u: Generator[U]): Generator[(T, U)] = for {
    x <- t
    y <- u
  } yield (x, y)

  val pairDesugared: Generator[(Int, Int)] = integers flatMap {
    x => integers map { y => (x, y) }
  }

  def triangles(width: Int): Generator[(Int, Int)] = for {
    x <- choose(0, width)
    y <- choose(0, x)
  } yield (x, y)

  def emptyLists = single(Nil)

  def nonEmptyLists = for {
    head <- integers
    tail <- lists
  } yield head :: tail

  def lists: Generator[List[Int]] = for {
    isEmpty <- booleans
    list <- if (isEmpty) emptyLists else nonEmptyLists
  } yield list


  trait Tree

  case class Inner(left: Tree, right: Tree) extends Tree

  case class Leaf(x: Int) extends Tree

  def leafs: Generator[Tree] = for {
    x <- integers
  } yield Leaf(x)


  def inners: Generator[Tree] = for {
    left <- trees
    right <- trees
  } yield {
    Inner(left, right)
  }

  def trees: Generator[Tree] =
    for {
      isLeaf <- booleans
      tree <- if (isLeaf) leafs else inners
    } yield tree

  trees.generate


  def test[T](r: Generator[T], noTimes: Int = 100)(test: T => Boolean) {
    for (_ <- 0 until noTimes) {
      val value = r.generate
      assert(test(value), "Test failed for: "+value)
    }
    println("Test passed "+noTimes+" times")
  }

  test(pairs(lists, lists)) {
    case (xs, ys) => (xs ::: ys).length > xs.length
  }
}