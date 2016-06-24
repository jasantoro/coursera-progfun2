case class Book(title: String, authors: List[String])

val books = Set(
  new Book("Structure and Interpretation of Computer Programs", List("Abelson, Harald", "Sussman, Gerald J.")),
  new Book("Introduction of Functional Programming", List("Bird, Richard", "Wadler, Phil")),
  new Book("Effective Java", List("Bloch, Joshua")),
  new Book("Effective Java 2", List("Bloch, Joshua")),
  new Book("Java Puzzlers", List("Bloch, Joshua", "Gafter, Neal")),
  new Book("Programming in Scala", List("Odersky, Martin", "Spoon, Lex", "Venners, Bill"))
)

for (b <- books; a <- b.authors if a startsWith("Bloch,"))
  yield b.title

for (b <- books if (b.title.indexOf("Program") >= 0))
  yield b.title

for {
  b1 <- books
  b2 <- books
  if (b1.title < b2.title)
  a1 <- b1.authors
  a2 <- b2.authors
  if (a1 == a2)
} yield a1