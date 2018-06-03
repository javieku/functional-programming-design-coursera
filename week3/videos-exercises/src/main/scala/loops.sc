object session {
  def REPEAT(command: => Unit)(condition: => Boolean): Unit = {
    command
    if (condition)
      ()
    else
      REPEAT(command)(condition)
  }

  var x = 1
  REPEAT {
    x += 1
  }(x == 10)


  REPEAT {
    x += 1
  }UNTIL(x == 10)
}