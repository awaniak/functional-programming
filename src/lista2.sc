//Adam Waniak 228124

//Zad 1
print("Zad 1")
def take[A](n: Int, xs: List[A]): List[A] =
  xs match {
    case h :: t => if (n > 0) h :: take(n - 1, t) else Nil
    case Nil => Nil
  }

take(2, List(1, 2, 3, 5, 6))
take(2, List(1, 2, 3, 5, 6)) == List(1, 2)
take(-2, List(1, 2, 3, 5, 6)) == Nil
take(8, List(1, 2, 3, 5, 6)) == List(1, 2, 3, 5, 6)
take(12, List()) == Nil
take(1, List(-1, -5)) == List(-1)

//Zad 2
print("Zad 2")
def drop[A](n: Int, xs: List[A]): List[A] =
  xs match {
    case h :: t => if (n > 0) drop(n - 1, t) else xs
    case Nil => xs
  }

drop(2, List(1, 2, 3, 5, 6)) == List(3, 5, 6)
drop(-2, List(1, 2, 3, 5, 6)) == List(1, 2, 3, 5, 6)
drop(8, List(1, 2, 3, 5, 6)) == Nil
drop(8, List()) == Nil
drop(1, List(-1, -5)) == List(-5)

//Zad 3

print("Zad 3")

def reverse[A](xs: List[A]): List[A] = {
  def inner(acc: List[A], rem: List[A]): List[A] = rem match {
    case h :: t => inner(h :: acc, t)
    case Nil => acc
  }
  inner(Nil, xs)
}

reverse(List("Ala", "ma", "kota")) == List("kota", "ma", "Ala")
reverse(Nil) == Nil
reverse(List("Ala")) == List("Ala")

//Zad 4

print("Zad 4")

def replicate(xs: List[Int]): List[Int] = {
  def duplicate(value: Int, iter: Int): List[Int] =
    if (iter <= 0) Nil else value :: duplicate(value, iter - 1)

  xs match {
    case h :: t => duplicate(h, h) ::: replicate(t)
    case Nil => Nil
  }
}

replicate(List(1, 0, 4, -2, 3)) == List(1, 4, 4, 4, 4, 3, 3, 3)
replicate(List(1, 0, 4)) == List(1, 4, 4, 4, 4)
replicate(Nil) == Nil



// Zad 5

print("Zad 5")

def root3(a: Double): Double = {
  def improve(xi: Double): Double =
    if (math.abs(xi * xi * xi - a) <= 10E-15 * math.abs(a)) xi
    else improve(xi + (a / (xi * xi) - xi) / 3)

  improve(if (a > 1) a / 3 else a)
}

root3(-8.0) == -2.0
root3(8.0) == 2.0
root3(-10) == -2.154434690031884
root3(0) == 0
root3(-1) == -1
