// Adam Waniak
sealed trait BT[+A]

case object Empty extends BT[Nothing]

case class Node[+A](elem: A, left: BT[A], right: BT[A]) extends BT[A]

val t = Node(1, Node(2, Empty, Node(3, Empty, Empty)), Empty)


// Zad 1.

print("Zad 1")

def sumBT[A](bt: BT[Int]): Int =
  bt match {
    case Node(elem, left, right) => elem + sumBT(left) + sumBT(right)
    case Empty => 0
  }

sumBT(t) == 6

// Zad 2.
print("Zad 2")

def foldBT[A, B](f: A => ((B, B) => B))(acc: B)(bt: BT[A]): B = {
  bt match {
    case Node(elem, left, right) => f(elem)(foldBT(f)(acc)(left), foldBT(f)(acc)(right))
    case Empty => acc
  }
}

//Zad 3.
print("Zad 3")
print("a")
def sumBTfold(bt: BT[Int]): Int = foldBT((acc: Int) => (a: Int, b: Int) => acc + a + b)(0)(bt)

sumBTfold(t) == 6
print("b")

def inorderBTfold[A](bt: BT[A]): List[A] = foldBT((root: A) => (l: List[A], r: List[A]) => l ::: (root :: r))(List[A]())(bt)

inorderBTfold(t) == List(2, 3, 1)

val x = Node(1, Node(2, Node(3, Empty, Empty), Node(3, Empty, Empty)), Node(2, Node(3, Empty, Empty), Empty))
inorderBTfold(x) == List(3, 2, 3, 1, 3, 2)

// Zad. 4
print("Zad 4")

def mapBT[A, B](f: A => B)(tree: BT[A]): BT[B] = foldBT[A, BT[B]]((x: A) => (y: BT[B], z: BT[B]) => Node(f(x), y, z))(Empty)(tree)

mapBT((v: Int) => 2 * v)(t) == Node(2, Node(4, Empty, Node(6, Empty, Empty)), Empty)


// Zadanie 5

sealed trait Graphs[A]

case class Graph[A](succ: A => List[A]) extends Graphs[A]

val g = Graph((i: Int) =>
  i match {
    case 0 => List(3)
    case 1 => List(0, 2, 4)
    case 2 => List(1)
    case 3 => List(5)
    case 4 => List(0, 2)
    case 5 => List(3)
    case n => throw
      new NoSuchElementException("Graph g: node" + n + "doesn't exist")
  })

def pathExists[A](g: Graph[A])(from: A, to: A): Boolean = {
  def search(visited: List[A])(toVisit: List[A]): Boolean =
    toVisit match {
      case h :: tail => (h == to) || (if (visited contains h) search(visited)(tail)
      else search(h :: visited)(tail ::: (g succ h)))
      case Nil => false
    }

  search(Nil)(List(from))
}

pathExists(g)(4, 1)
!pathExists(g)(0, 4)
pathExists(g)(1, 2)
!pathExists(g)(3, 0)

