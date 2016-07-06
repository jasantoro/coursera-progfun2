def iterate(n: Int, f: Int => Int, x: Int): Int =
  if(n == 0) x else iterate(n - 1, f, f(x))
def square(x: Int): Int = x * x

iterate(1, square, 4)

class BankAccount {
  private var balance = 0
  def deposit(amount: Int): Unit = {
    if(amount > 0) balance = balance + amount
  }
  def withdraw(amount: Int): Int = {
    if(0 < amount && amount <= balance) {
      balance = balance - amount
      balance
    } else throw new Error("insufficient funds")
  }
}

val account = new BankAccount
account.deposit(50)
account.withdraw(20)
account.withdraw(20)

def cons[T](hd: T, tl: => Stream[T]) = new Stream[T] {
  override def head = hd
  override def tailDefined = false
  private var tlOpt: Option[Stream[T]] = None
  override def tail: Stream[T] = tlOpt match {
    case Some(x) => x
    case None => tlOpt = Some(tl); tail
  }
}