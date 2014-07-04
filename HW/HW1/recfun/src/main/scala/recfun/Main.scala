package recfun
import common._

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if (c == 0 || c == r) 1
    else {
      pascal(c - 1, r - 1) + pascal(c, r - 1)
    }
  }

  /**
   * Exercise 2
   */
  /**
  def balance(chars: List[Char]): Boolean = {
    def balance2(chars: List[Char], open1: Int, open2: Int, open3: Int): Boolean = {
      if (chars.isEmpty && open1 == 0 &&
        open2 == 0 && open3 == 0) true
      else if (open1 < 0 || open2 < 0 || open3 < 0) false
      else {
        if (chars.head == '(')
          balance2(chars.tail, open1 + 1, open2, open3)
        else if (chars.head == '{')
          balance2(chars.tail, open1, open2 + 1, open3)
        else if (chars.head == '[')
          balance2(chars.tail, open1, open2, open3 + 1)
        else if (chars.head == ')')
          balance2(chars.tail, open1 - 1, open2, open3)
        else if (chars.head == '}')
          balance2(chars.tail, open1, open2 - 1, open3)
        else if (chars.head == ']')
          balance2(chars.tail, open1, open2, open3 - 1)
        else
          balance2(chars.tail, open1, open2, open3)
      }
    }
    balance2(chars, 0, 0, 0)
  }
  **/
  def balance(chars: List[Char]): Boolean = {
    def balance2(chars: List[Char], open: Int): Boolean = {
      if (chars.isEmpty && open == 0) true
      else if (open < 0) false
      else {
        if (chars.head == '(')
          balance2(chars.tail, open + 1)
        else if (chars.head == ')')
          balance2(chars.tail, open - 1)
        else
          balance2(chars.tail, open)
      }
    }
    if (chars.isEmpty) true
    else balance2(chars,0)
  }
      	
  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (coins.isEmpty || money < 0) 0
    else if (money == 0) 1
    else {
      countChange(money-coins.head, coins) +
      countChange(money, coins.tail)
    }
  }
}
