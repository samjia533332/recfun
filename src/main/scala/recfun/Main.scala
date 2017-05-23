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
      if (c == 0 || c == r) 1
      else pascal(c - 1, r - 1) + pascal(c, r - 1)
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      val numLeft = 0 // like a stack

      def balanceHelper(num:Int, currentChar:Char, leftList:List[Char]) : Boolean = {
        if (leftList.isEmpty) {
          if (currentChar == '(') false
          else if (currentChar == ')' && num == 1) true
          else if (currentChar == ')' && num != 1) false
          else if (num == 0) true
          else false
          // chars.isEmpty: Boolean returns whether a list is empty
          // chars.head: Char returns the first element of the list
          // chars.tail: List[Char] returns the list without the first element
        }
        else {
          if (currentChar == '(') balanceHelper(num+1,leftList.head,leftList.tail)
          else if (currentChar == ')' && num == 0) false
          else if (currentChar == ')' && num > 0) balanceHelper(num -1, leftList.head,leftList.tail)
          else balanceHelper(num, leftList.head,leftList.tail)
        }
      }
      if (chars.isEmpty) true
      else balanceHelper(numLeft,chars.head,chars.tail)
      // test cases:
      // 1. (if (zero? x) max (/ 1 x))
      // 2. I told him (that it’s not (yet) done). (But he wasn’t listening)
      // 3. :-)
      // 4. ())(
    }

    
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      def countHelper(total:Int, money:Int, coins:List[Int]):Int = { 
        // we need total to record the total value of note
        if (coins.isEmpty) 0
        else {
          if (coins.tail.isEmpty) {
            if (money < coins.head) 0
            else if (money > coins.head) countHelper(total,money - coins.head, coins)
            else 1
          }
          else {
            if (money < coins.head) countHelper(total,total, coins.tail)
            else if (money == coins.head) 1 + countHelper(total,total, coins.tail)
            else countHelper(total,money - coins.head, coins) + 
              countHelper(total, money - coins.head, coins.tail)
            // already take into consideration by adding term (total, money-coins.head, coins.tail)
          }
        }
      }
      countHelper(money,money,coins)
    }
}
