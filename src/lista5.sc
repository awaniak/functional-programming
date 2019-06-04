// Adam Waniak

//Zad 1.
print("Zad 1")

class MyPair[A, B](fst: A, snd: B) {
  override def toString: String = "(" + fst + ", " + snd + ")"
}

val para = new MyPair[Int, String](4, "ala")
para.toString == "(4, ala)"
val para2 = new MyPair[Any, Any]("zny", 5)
para2.toString == "(zny, 5)"

// Zad. 2
print("Zad 2")

class BankAccount(initialBalance: Double) {
  private var balance = initialBalance

  def checkBalance: Double = balance

  def deposit(amount: Double): Double = {
    balance += amount
    balance
  }

  def withdraw(amount: Double): Double = {
    balance -= amount
    balance
  }
}

// a)
class CheckingAccount(initialBalance: Double) extends BankAccount(initialBalance) {
  override def deposit(amount: Double): Double = super.deposit(amount - 1)

  override def withdraw(amount: Double): Double = super.withdraw(amount + 1)
}

val accountCheckingAccount = new CheckingAccount(10)
accountCheckingAccount.deposit(10) == 19
accountCheckingAccount.withdraw(10) == 8


// b)


class SavingsAccount(initialBalance: Double) extends BankAccount(initialBalance) {
  private var numOfOperations: Int = 0

  def earnMonthlyInterest(): Unit = {

    deposit(checkBalance * 0.02 / 100)
    numOfOperations = 0
  }

  override def deposit(amount: Double): Double = {
    numOfOperations += 1
    super.deposit(if (numOfOperations > 3) amount -1 else amount)
  }

  override def withdraw(amount: Double): Double = {
    numOfOperations += 1
    if (numOfOperations > 3) super.withdraw(amount + 1)
    else super.withdraw(amount)
  }
}

val savingsAccount = new SavingsAccount(10)
savingsAccount.deposit(5)
savingsAccount.deposit(5)
savingsAccount.deposit(5)
savingsAccount.deposit(5)
savingsAccount.checkBalance == 29

// Zad 3.

abstract class Zwierz(val imie: String = "bez imienia") {
  def rodzaj(): String

  def dajGlos(): String

  override def toString: String =  rodzaj() + " " + imie + " daje glos " + dajGlos()
}

class Pies(override val imie: String = "bez imienia") extends Zwierz(imie) {
  override def rodzaj(): String = "Pies"
  override def dajGlos() = "Hau Hau"
}

class Kot(override val imie: String = "bez imienia") extends Zwierz(imie) {
  def rodzaj(): String = "Kot"
  override def dajGlos() = "Miau"
}
val szramek = new Pies("szramek")
print(szramek.toString == "Pies szramek daje glos Hau Hau")
val kicia = new Kot("kicia")
print(kicia.toString == "Kot kicia daje glos Miau")

val kicia2 = new Kot()
print(kicia2.toString == "Kot bez imienia daje glos Miau")
val vec = Vector(szramek, kicia, kicia2)

object TestZwierza {
  def main(args: Array[String]): Unit = {
    val pies1 = new Pies("szramek")
    val kot1 = new Kot("kicia")
    val kot2 = new Kot()
    val vec = Vector(pies1, kot1, kot2)
    vec.foreach(zwierz => {
      print(zwierz.toString)
    })
  }
}


TestZwierza.main(Array())



