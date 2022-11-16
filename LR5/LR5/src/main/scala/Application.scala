package main.scala

object Application {

  var count = 0
  def countFunc: Int = {
    count += 1
    count
  }

  def decToN(n: Int)(x: Int): String = {
    def numToLetter(num: Int): String = {
      num match {
        case 10 => "A"
        case 11 => "B"
        case 12 => "C"
        case 13 => "D"
        case 14 => "E"
        case 15 => "F"
        case _ => num.toString
      }
    }
    def toN(x: Int): String = {
      if(x < n)
        numToLetter(x)
      else
        toN(x / n) + numToLetter(x % n)
    }
    toN(x)
  }

  def decToBin(x: Int): String = decToN(2)(x)

  def fun(start: Int): Int => List[Int] = {
    def f(len: Int): List[Int] = {
      var lst: List[Int] = Nil
      for (_ <- 1 to len) {
        lst = start + scala.util.Random.nextInt(10) - 5 :: lst
      }
      lst
    }
    f
  }



  def main (args: Array[String]): Unit = {

//    println(countFunc)
//    println(countFunc)
//    println(countFunc)
//    println(countFunc)

    println(decToN(2)(42))
    println(decToN(3)(42))
    println(decToN(4)(42))
    println(decToN(16)(42))

    println(decToBin(42))

//      scala.util.Random.setSeed(42)
//
//      val funStart20 = fun(20)
//      println(funStart20(10))
  }
}
