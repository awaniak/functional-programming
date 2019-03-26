//  xs.foldLeft(false)((acc, cur) => acc || p(cur) )
sealed trait BT[+A]

case object Empty extends BT[Nothing]

case class Node[+A](elem: A, left: BT[A], right: BT[A]) extends BT[A]

val t = Node(1, Node(2, Empty, Node(3, Empty, Empty)), Empty)

// Zadanie 1

def sumaBT[A](bt: BT[Int]): Int = bt match {
  case Node(elem, left, right) => elem + sumaBT(left) + sumaBT(right)
  case Empty => 0
}

sumaBT(t)

// Zadanie 2

def foldBT[A, B](f: A => (B, B) => B)(acc: B)(bt: BT[A]): B = bt match {
  case Node(value, l, r) => f(value)(foldBT(f)(acc)(l), foldBT(f)(acc)(r))
  case Empty => acc
}

// Zadanie 3

//A
def sumBTfold[A](bt: BT[Int]): Int = foldBT((acc: Int) => (a: Int, b: Int) => acc + a + b)(0)(bt)

sumBTfold(t) == 6

//B

def inorderBTfold[A](bt: BT[A]): List[A] = foldBT((root: A) => (l: List[A], r: List[A]) => l ::: (root :: r))(List[A]())(bt)

inorderBTfold(t)

val x = Node(1, Node(2, Node(3, Empty, Empty), Node(3, Empty, Empty)), Node(2, Node(3, Empty, Empty), Empty))
inorderBTfold(x)

// Zadanie 4

def mapBT[A,B](f: A => B)(tree: BT[A]): BT[B] = foldBT[A, BT[B]]((x: A) => (y:BT[B], z:BT[B]) => Node(f(x), y, z))(Empty)(tree)
mapBT((v: Int) => 2 * v)(t: BT[Int])

sealed trait Graphs[A]

case class Graph[A](succ: A => List[A]) extends Graphs[A]

val g = Graph((i: Int) =>
  i match {
    case 0 => List(3)
    case 1 => List(0, 2, 4)
    case 2 => List(1)
    case 3 => Nil
    case 4 => List(0, 2)
    case n => throw
      new NoSuchElementException("Graph g: node" + n + "doesn't exist")
  })


// Zadanie 5

def pathExists[A](g: Graph[A])(from: A, to: A): Boolean = {
  def search(visited: List[A])(toVisit: List[A]): List[A] =
    toVisit match {
      case h :: tail => if (visited contains h) search(visited)(tail)
      else h :: search(h :: visited)(tail ::: (g succ h))
      case Nil => Nil
    }
  search(Nil)(List(from)) contains to
}

pathExists(g)(4,1) == true
pathExists(g)(0,4) == false
pathExists(g)(1,2) == true