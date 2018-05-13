package week1

object session {

  /*** For-expression are useful for querying the database (Slick) ***/
  case class Book(title: String, authors: List[String])


  //A mini-database - implemented as a Set
  val booksSet: Set[Book] = Set(
    Book(title = "Structure and Interpretation of Computer Programs",
      authors = List("Abelson, Harald", "Sussman, Gerald J.")),
    Book(title = "Introduction to Functional Programming",
      authors = List("Bird, Richard", "Wadler, Phil")),
    Book(title = "Effective Java",
      authors = List("Bloch, Joshua")),
    Book(title = "Effective Java 2",
      authors = List("Bloch, Joshua")),
    Book(title = "Java Puzzlers",
      authors = List("Bloch, Joshua", "Gafter, Neal")),
    Book(title = "Programming in Scala",
      authors = List("Ordersky, Martin", "Spoon, Lex", "Venners, Bill"))
  )

  //A mini-database - implemented as a List
  val booksList: List[Book] = List(
    Book(title = "Structure and Interpretation of Computer Programs",
      authors = List("Abelson, Harald", "Sussman, Gerald J.")),
    Book(title = "Introduction to Functional Programming",
      authors = List("Bird, Richard", "Wadler, Phil")),
    Book(title = "Effective Java",
      authors = List("Bloch, Joshua")),
    Book(title = "Effective Java 2",
      authors = List("Bloch, Joshua")),
    Book(title = "Java Puzzlers",
      authors = List("Bloch, Joshua", "Gafter, Neal")),
    Book(title = "Programming in Scala",
      authors = List("Ordersky, Martin", "Spoon, Lex", "Venners, Bill"))
  )

  for {
    b1 <- booksList
    b2 <- booksList
    if b1.title != b2.title
    a1 <- b1.authors
    a2 <- b2.authors
    if a1 == a2
  } yield a1
  //  res0: List[String] = List(Bloch, Joshua, Bloch, Joshua, Bloch, Joshua, Bloch, Joshua, Bloch, Joshua, Bloch, Joshua)

  for {
    b1 <- booksSet
    b2 <- booksSet
    if b1.title != b2.title
    a1 <- b1.authors
    a2 <- b2.authors
    if a1 == a2
  } yield a1
  //  res1: scala.collection.immutable.Set[String] = Set(Bloch, Joshua)


  for {
    b1 <- booksList
    b2 <- booksList
    if b1.title < b2.title
    a1 <- b1.authors
    a2 <- b2.authors
    if a1 == a2
  } yield a1

  for(b <- booksSet; a <- b.authors if a startsWith "Bird") yield b.title
  //> res11: scala.collection.immutable.Set[String] = Set(Introduction to Functional Programming)

  booksSet.flatMap( book => book.authors.withFilter( author => author.startsWith("Bird") )
    .map( _ => book.title ))
}
