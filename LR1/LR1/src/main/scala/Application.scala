package main.scala

object Application {
  // Функция возвращает новый список, в котором каждый элемент является
  // суммой предыдущего элемента нового списка и текущего элемента входного списка
  def cumsumList(lst: List[Int], sum: Int = 0): List[Int] = {
    if (lst == Nil) Nil
    else sum + lst.head :: cumsumList(lst.tail, sum + lst.head)
  }

  def cumsumListT(lst: List[Int]): List[Int] = {
    def cumsumListAcc(_lst: List[Int], sum: Int, acc: List[Int]): List[Int] = {
      if (_lst == Nil) acc
      else cumsumListAcc(_lst.tail, sum + _lst.head, sum + _lst.head :: acc)
    }

    cumsumListAcc(lst, 0, Nil).reverse
  }

  def main (args: Array[String]): Unit = {
    scala.util.Random.setSeed(42)
    var cnt = 0
    var llst :List[Int] = List()
    while(cnt<10000) {
      llst = scala.util.Random.nextInt(10) :: llst
      cnt += 1
    }

    //println(cumsumList(llst))
    println(cumsumListT(llst))
  }
}
