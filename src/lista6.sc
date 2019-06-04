// Adam Waniak
// Zad. 1
def whileLoop(condition: => Boolean)(expr: => Unit): Unit =
  if (condition) {
    expr
    whileLoop(condition)(expr)
  }

var count = 0
whileLoop(count < 5){
  print(count)
  count += 1
}


// Zad. 2

def lrepeat[A](k: Int)(stream: Stream[A]): Stream[A] = {
  def lrepeatHelper(iter: Int)(resultStream: Stream[A]): Stream[A] =
    resultStream match {
      case h #:: t => if (iter > 0) h #:: lrepeatHelper(iter - 1)(resultStream) else lrepeat(k)(t)
      case Stream.Empty => Stream.Empty
    }

  lrepeatHelper(k)(stream)
}

(lrepeat(3)(Stream.from(1)) take 12).toList == List(1, 1, 1, 2, 2, 2, 3, 3, 3, 4, 4, 4)
lrepeat(3)(Stream.Empty).toList == List()
lrepeat(3)(Stream(1, 2)).toList == List(1, 1, 1, 2, 2, 2)


// Zad. 3
sealed trait lBT[+A]
case object LEmpty extends lBT[Nothing]
case class LNode[+A](elem: A, left: () => lBT[A], right: () => lBT[A]) extends lBT[A]


def lBreadth[A](ltree: lBT[A]) : Stream[A] = {

  def llBreadth[A](q: List[lBT[A]]) : Stream[A] = {
    q match  {
      case LEmpty :: t => llBreadth(t)
      case LNode(elem, left, right)::t => elem #:: llBreadth(t ::: left() :: right() :: Nil)
      case Nil => Stream.Empty
    }
  }
  llBreadth(List(ltree))
}

val t2 =  LNode(1,
  () => LNode(2,
    () => LNode(4,
      () => LEmpty, () => LEmpty),
    () => LEmpty),
  () => LNode(3,
    () => LNode(5,
      () => LEmpty,
      () =>LNode(6,
        () =>LEmpty,() => LEmpty)),
    () => LEmpty))


lBreadth(t2).toList == List(1, 2, 3, 4, 5, 6)
lBreadth(LEmpty).toList == List()

def lTree (n: Int) : lBT[Int] = {
  LNode(n, () => lTree(2*n), () => lTree(2*n + 1))
}

lBreadth(lTree(1)).take(10).toList == List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
lBreadth(lTree(2)).take(7).toList == List(2, 4, 5, 8, 9, 10, 11)

