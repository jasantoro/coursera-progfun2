def mapFun[T, U](xs: List[T], f: T => U): List[U] =
  for (x <- xs) yield f(x)

def flatMap[T, U](xs: List[T], f: T => Iterable[U]): List[U] =
  for (x <- xs; y <- f(x)) yield y

def filter[T](xs: List[T], p: T => Boolean): List[T] =
  for (x <- xs if p(x)) yield x

val n = 7

def isPrime(n: Int) = (2 until n) forall (n % _ != 0)

for {
  i <- 1 until n
  j <- 1 until i
  if isPrime(i + j)
} yield (i, j)

(1 until n).flatMap(i =>
  (1 until i).withFilter(j => isPrime(i +j))
  .map(j => (i, j)))

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

books.flatMap(b => for (a <- b.authors if a.startsWith("Bloch,")) yield b.title)

books.flatMap(b => for (a <- b.authors.withFilter(a => a.startsWith("Bloch,"))) yield b.title)

books.flatMap(b => b.authors.withFilter(a => a.startsWith("Bloch,")).map(y => y.title))