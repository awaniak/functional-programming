// Adam Waniak

// Zad 1.

print("Zad 1")
print("a")
def exist[A](xs: List[A])(p: A => Boolean): Boolean =
  xs match {
    case h::t =>  (p(h)) || exist(t)(p)
    case Nil => false
  }


exist(List(5,1,2,3))(_ == 2)
!exist(List())(_ == 2)

print("b")
def existFoldLeft[A](xs: List[A])(p: A => Boolean): Boolean =
  xs.foldLeft(false)(_ || p(_))


existFoldLeft(List(5,1,2,3))(_ == 2)
!existFoldLeft(List())(_ == 2)

print("c")
def existFoldRight[A](xs: List[A])(p: A => Boolean): Boolean =
  xs.foldRight(false)(p(_) || _)


existFoldRight(List(5,1,2,3))(_ == 2)
!existFoldRight(List())(_ == 2)


//Zad 2.
print("Zad 2.")

def filter[A](xs: List[A])(p: A => Boolean): List[A] =
  xs.foldRight(List(): List[A])((cur, acc) =>
    if(p(cur)) cur :: acc else acc)


filter (List(2,7,1,3,7,8,4,1,6,9)) (_ > 3) == List(7, 7, 8, 4, 6, 9)
filter (List(2,7,1,3,7,8,4,1,6,9)) (_ > 10) == List()
filter(List())(_ == 10) == List()

//Zad 3.
print("Zad 3")
print("a")

def  remove1[A](xs: List[A])(p: A => Boolean): List[A] =
  xs match {
    case h :: t => if (p(h)) t else h :: remove1(t)(p)
    case Nil => Nil
  }


remove1(List(1,2,3,2,5)) (_ == 2) == List(1, 3, 2, 5)
remove1(List(1,2,3,2,5)) (_ == 6) == List(1,2,3,2,5)
remove1(List(1,5,6,7,8,9)) (_ == 7) == List(1, 5, 6, 8, 9)
remove1(List()) (_ == 6) == List()

print("b")

def  remove1Rec[A](xs: List[A])(p: A => Boolean): List[A] = {
  def inner[A](xs: List[A], acc: List[A])(p: A => Boolean): List[A] = {
    xs match {
      case h :: t => if (p(h)) t.reverse_:::(acc) else  inner(t, h :: acc)(p)
      case Nil => acc.reverse
    }

  }
  inner(xs, List())(p)
}

remove1Rec(List(1,2,3,2,5)) (_ == 2) == List(1, 3, 2, 5)
remove1Rec(List(1,2,3,4,5)) (_ == 2) == List(1, 3, 4, 5)
remove1Rec(List(1,2,3,2,5)) (_ == 6) == List(1,2,3,2,5)
remove1Rec(List(1,5,6,7,8,9)) (_ == 7) == List(1, 5, 6, 8, 9)
remove1Rec(List()) (_ == 6) == List()

//Zad 4.

print("Zad 4")

def splitAt[A](xs:List[A])(n:Int):(List[A],List[A]) = {
  def splitAtAcc(xs: List[A])(acc: List[A])(n: Int): (List[A], List[A]) =
    xs match {
      case h :: t => if (n > 0) splitAtAcc(t)(h::acc)(n-1) else (acc.reverse, xs)
      case Nil => (acc.reverse, xs)
    }
  splitAtAcc(xs)(List())(n)
}


splitAt (List('a','b','c','d','e')) (2) == (List('a', 'b'), List('c', 'd', 'e'))
splitAt (List('a','b','c','d','e')) (10) == (List('a','b','c','d','e'), List())
splitAt (List()) (10) == (List(),List())


