// Adam Waniak
import java.util.NoSuchElementException
// Zad 1.

def suma(xs: List[Double]): Double =
  if (xs == Nil) 0.0
  else xs.head + suma(xs.tail)


suma(Nil) == 0.0
suma(List(-1, 2, 3)) == 4.0
suma(List(5.6)) == 5.6

// Zad 2.

def ends[A](xs: List[A]): (A, A) = {

  def endElement[A](xs: List[A]): A =
    if (xs.tail == Nil) xs.head
    else endElement(xs.tail)

  if (xs == Nil) throw new IllegalArgumentException()
  else (xs.head, endElement(xs))
}

ends(List(1, 2, 3, 5)) == (1, 5)
ends(List(1)) == (1, 1)
ends(List("a", "b", "c")) == ("a", "c")
try
  ends(Nil)
catch {
  case _: IllegalArgumentException => true
}


//Zad 3.

def posortowana(xs: List[Int]): Boolean =
  xs == Nil || xs.tail == Nil || xs.head <= xs.tail.head && posortowana(xs.tail)

posortowana(List(1, 2, 3, 4, 5))
posortowana(List(1, 3, 3, 5, 6, 7))
posortowana(List())
posortowana(List(3))
!posortowana(List(9, 0))

//Zad 4.

val glue: (List[String], String) => String = (xs, sep) =>
  if (xs == Nil) ""
  else if (xs.tail == Nil) xs.head
  else xs.head + sep + glue(xs.tail, sep)

glue(List("a", "b"), ",") == "a,b"
glue(List(), ",") == ""
glue(List("a"), ",") == "a"
glue(List("a", "b", "c", "d"), ",") == "a,b,c,d"





