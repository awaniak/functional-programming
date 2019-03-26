def myMap[A, B](xs: List[A], pred: A => B): List[B] = {
  if (xs == Nil) return Nil
  pred(xs.head) :: myMap(xs.tail, pred)
}

myMap(List(1,2,3), (x: Int) => x * 2)
myMap(List(), (x: Int) => x * 2)
myMap(List("123", "asbc", "23"), (x: String) => x.getBytes)

def myReduce[A, B](xs: List[A], pred: (A, B) => B, firstElement: B): B = {
  if (xs == Nil) return firstElement
  pred(xs.head, myReduce(xs.tail, pred, firstElement))
}

myReduce(List(1,2,3), (a: Int, b: Int) => a + b, 0)

myReduce(List("a","vds", "csads"), (a: String, b: Int) => b + a.length, 0)

poprawka:
  lista1p1.sc



