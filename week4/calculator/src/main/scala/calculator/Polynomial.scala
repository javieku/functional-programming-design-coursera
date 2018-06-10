package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
                   c: Signal[Double]): Signal[Double] = {
    Signal(b() * b() - (4 * a() * c()))
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
                       c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    //(-b ± √Δ) / 2a

    val delta = computeDelta(a,b,c)
    Signal
    {
      val x = delta()
      if (x < 0)
        Set()
      else {
        val root1 = (-b() + Math.sqrt(x)) / (2 * a())
        val root2 = (-b() - Math.sqrt(x)) / (2 * a())
        Set(root1, root2)
      }
    }

  }
}
