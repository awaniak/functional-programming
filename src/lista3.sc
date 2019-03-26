// Adam Waniak

// Zad 1.
// Napisz funkcję exists: [A] (xs: List[A]) (p: A => Boolean) Boolean.
// exists (xs) (p) ma wartość logiczną zdania „xxs.p(x)”
// np. exists (List(5,1,2,3)) (_ == 2) == true
// Należy napisać trzy wersje tej funkcji:
// a) z wykorzystaniem dopasowania do wzorca i rekursji,
// b) z wykorzystaniem metody List.foldLeft i
// c) z wykorzystaniem metody List.foldRight.

print("Zad 1")
print("a")
def exist[A] (xs: List[A]) (p: A => Boolean): Boolean = {
  xs match {
    case h::t => if (p(h)) true else exist(t)(p)
    case Nil => false
  }
}

exist(List(5,1,2,3))(_ == 2)
!exist(List())(_ == 2)

print("b")
def existFoldLeft[A](xs: List[A]) (p: A => Boolean): Boolean = {
//  xs.foldLeft(false)((acc, cur) => acc || p(cur) )
  xs.foldLeft(false)(_ || p(_))
}

existFoldLeft(List(5,1,2,3))(_ == 2)
!existFoldLeft(List())(_ == 2)

print("c")
def existFoldRight[A](xs: List[A]) (p: A => Boolean): Boolean = {
  xs.foldRight(false)(p(_) || _)
}

existFoldRight(List(5,1,2,3))(_ == 2)
!existFoldRight(List())(_ == 2)


//Zad 2.
print("Zad 2.")

def filter[A](xs: List[A])(p: A => Boolean): List[A] = {
  xs.foldRight(List(): List[A])((cur, acc) =>
    if(p(cur)) cur :: acc else acc)
}

filter (List(2,7,1,3,7,8,4,1,6,9)) (_ > 3) == List(7, 7, 8, 4, 6, 9)
filter (List(2,7,1,3,7,8,4,1,6,9)) (_ > 10) == List()

//Zad 3.
print("Zad 3")

def  remove1[A](xs: List[A])(p: A => Boolean): List[A] = {
  xs match {
    case h :: t => if (p(h))
  }
}


remove1(List(1,2,3,2,5)) (_ == 2) == List(1, 3, 2, 5)




