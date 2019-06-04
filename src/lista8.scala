// Adam Waniak

import reflect.ClassTag

class FullException(msg: String) extends Exception(msg)

abstract class MyQueueMut[E] {
  @throws[FullException]
  def enqueue(x: E): Unit

  def dequeue: Unit

  @throws[NoSuchElementException]
  def first: E

  def isEmpty: Boolean

  def isFull: Boolean
}

class QueueMut[E: ClassTag](val capacity: Int = 1000) extends MyQueueMut[E] {

  private val sizePlus = capacity + 1
  private val q: Array[E] = new Array[E](sizePlus)
  private var f, r = 0: Int

  override def enqueue(x: E): Unit =
    if (this.isFull)
      throw new FullException("Queue is full")
    else {
      q(r) = x
      r = (r + 1) % sizePlus
    }


  override def dequeue: Unit =
    if (this.isEmpty)
      ()
    else
      f = (f + 1) % sizePlus


  override def first: E =
    if (this.isEmpty)
      throw new NoSuchElementException("Queue is empty")
    else
      q(f)

  override def isEmpty: Boolean =
    f == r

  override def isFull: Boolean =
    f == (r + 1) % sizePlus

  override def toString: String = {
    q.toList.mkString(", ") + "     f = " + f + "  r = " + r
  }
}

object QueueMut {

  def empty[E: ClassTag](capacity: Int = 1000): QueueMut[E] = new QueueMut[E](capacity)

  def apply[E: ClassTag](xs: E*): QueueMut[E] = {
    val q: QueueMut[E] = QueueMut.empty(xs.length)
    for (x <- xs)
      q.enqueue(x)
    q
  }
}


object Lista8 {

  def main(args: Array[String]): Unit = {
    var q = new QueueMut[Int](3)
    var q2: QueueMut[Int] = QueueMut.empty(3)
    var q3 = QueueMut(1, 2, 3, 4)
    println(q.toString)
    println(q2.toString)
    println(q3.toString)

    q.enqueue(1)
    q.enqueue(2)
    q.enqueue(3)
    println(q.isFull==true)
    println(q.isEmpty==false)
    println(q.first == 1)
    q.dequeue
    println(q.first == 2)
    q.dequeue
    q.dequeue
    println(q.isEmpty==true)
    println(q.isFull==false)
    q.enqueue(5)
    println(q.first == 5)
    q.dequeue
    q.enqueue(6)
    println(q.first == 6)

  }
}