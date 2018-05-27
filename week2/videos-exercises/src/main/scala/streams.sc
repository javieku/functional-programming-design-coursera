object session
{

  def StreamRange(lo : Int, hi : Int) : Stream[Int] =
  {
    print(lo + " ")
    if (lo >= hi) Stream.empty
    else Stream.cons(lo, StreamRange(lo+1, hi) )
  }

  StreamRange(1, 10).take(3).toList
}