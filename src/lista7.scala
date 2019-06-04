// Adam Waniak
//Zad. 1
class MyQueue[+A] private (private val out: List[A], private val in: List[A]) {

  def this() {
    this(Nil, Nil)
  }

  def isEmpty: Boolean = out.isEmpty

  def enqueue[B >: A](elem: B): MyQueue[B] =
    if (out.nonEmpty) new MyQueue(out, elem :: in)
    else new MyQueue(elem :: in, Nil)

  def dequeue(): MyQueue[A] =
    out match {
      case _ :: b :: t => new MyQueue(b :: t, in)
      case _ :: Nil => new MyQueue(in.reverse, Nil)
      case Nil => this
    }

  def first(): A = out.head

  def firstOption(): Option[A] = {
    out match {
      case h :: _ => Some(h)
      case _ => None
    }
  }

  override def toString: String = (out ++ in.reverse) mkString ", "
}

object MyQueue {
  def empty(): Unit = new MyQueue()

  def apply[A](elements: A*): MyQueue[A] = new MyQueue(elements.toList, Nil)
}

sealed trait BT[+A]

case object Empty extends BT[Nothing]

case class Node[+A](elem: A, left: BT[A], right: BT[A]) extends BT[A]


object lista7 {
  def main(args: Array[String]): Unit = {
    var q = MyQueue(1, 2, 3)
    var q2 = new MyQueue
    var q3 = MyQueue.empty

    q = q.enqueue(4)
    println(q.toString == "1, 2, 3, 4")
    q = q.dequeue()
    println(q.toString == "2, 3, 4")
    println(q.first() == 2)


    q = MyQueue(1, 2, 3)
    println(q.first() == 1) // 1
    q = q.dequeue()
    println(q.first() == 2) // 2
    q = q.dequeue()
    println(q.first() == 3) // 3
    println(q.firstOption().contains(3)) // Some(3)
    q = q.dequeue()
    println(q.isEmpty) // True

    println(q.firstOption() == Option.empty) // Empty
    println("Breadth BT testing")
    val t1 = Node(1, Node(2, Empty, Node(3, Empty, Empty)), Empty)
    val t2 = Node(1, Node(2, Node(8, Node(9, Empty, Node(10, Empty, Empty)), Empty), Node(3, Empty, Empty)), Node(5, Node(1, Empty, Empty), Empty))
    println(breadthBT(t1) == List(1, 2, 3))
    println(breadthBT(t2) == List(1, 2, 5, 8, 3, 1, 9, 10))
  }

  def breadthBT[A](tree: BT[A]): List[A] = {
    def helpBreadthBT(q: MyQueue[BT[A]]): List[A] = q.firstOption() match {
        case Some(Node(v, l, r)) => v :: helpBreadthBT(q.dequeue().enqueue(l).enqueue(r))
        case Some(Empty) => helpBreadthBT(q.dequeue())
        case _ => Nil
      }
    helpBreadthBT(MyQueue(tree))
  }
}
