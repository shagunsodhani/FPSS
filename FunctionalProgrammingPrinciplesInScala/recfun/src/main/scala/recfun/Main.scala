package recfun

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
    if (r < 0)
      0
    else if (c < 0) {
      0
    } else if (r == c)
      1
    else if (r == 0)
      1
    else if (c == 0)
      1
    else
      pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def balanceCharAux(chars: List[Char], count: Int): Boolean = {
      if (count < 0)
        false
      else if (chars.isEmpty)
        count == 0
      else if (chars.head == '(')
        balanceCharAux(chars.tail, count + 1)
      else if (chars.head == ')')
        balanceCharAux(chars.tail, count - 1)
      else
        balanceCharAux(chars.tail, count)
    }
    balanceCharAux(chars, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    
    if (money < 0) 
      0
    else if (money == 0)
      1
    else if (coins.isEmpty) {
      0
    }
    else
      countChange(money - coins.head, coins) + countChange(money, coins.tail)
  }
}
