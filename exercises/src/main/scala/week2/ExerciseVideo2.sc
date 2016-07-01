def isPrime(n: Int): Boolean = (2 until n) forall (n % _ != 0)
def secondPrime(from: Int, to: Int): Int = nthPrime(from, to, 2)
def nthPrime(from: Int, to: Int, n: Int): Int =
  if (from >= to) throw new Error("no prime")
  else if (isPrime(from))
    if (n == 1) from else nthPrime(from + 1, to, n - 1)
  else nthPrime(from + 1, to, n)

((1000 to 10000) filter isPrime)(1)

val xs = Stream.cons(1, Stream.cons(2, Stream.empty))
Stream(1, 2, 3)
(1 to 100).toStream

def streamRange(lo: Int, hi: Int): Stream[Int] = {
  print(lo + " ")
  if(lo >= hi) Stream.empty
  else Stream.cons(lo, streamRange(lo + 1, hi))
}

((1000 to 10000).toStream filter isPrime)(1)

streamRange(1, 10).take(3).toList

(streamRange(1000, 10000) filter isPrime) apply 1