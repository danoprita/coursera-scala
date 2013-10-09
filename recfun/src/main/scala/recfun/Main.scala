package recfun
import common._
import scala.collection.immutable.List._

object Main {
  def main(args: Array[String]) {
    testCountChange()
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if (c == 0 || c == r) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def loop(opened: Int, closed: Int, list: List[Char]): Boolean = {
      if (list.isEmpty && opened == closed) true
      else if (list.isEmpty || closed > opened) false
      else {
        if (list.head == '(') loop(opened + 1, closed, list.tail)
        else if (list.head == ')') loop(opened, closed + 1, list.tail)
        else loop(opened, closed, list.tail)
      }
    }
    loop(0, 0, chars)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0) 1
    else if (money < 0 || coins.isEmpty) 0
    else countChange(money, coins.tail) + countChange(money - coins.head, coins)
  }

  def printPascalTriangle() = {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  def testBalance() = {
    println("Balance")
    val list: List[Char] = "((this)) i(s a) string".toList;
    println(balance(list))
  }

  def testCountChange() = {
    println("Count change");
    println(countChange(2, List(1, 2)))
  }
}
