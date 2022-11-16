package main.scala

object Application {

  def division(numbers: List[(Int, Int)]): List[Option[Double]] = {
    numbers match {
      case Nil => Nil
      case head :: tail =>
        val res = head match {
          case (_, 0) => None
          case _ => Some((head._1).toDouble / head._2)
        }
        res :: division(tail)
    }
  }

  def doubleListToStr(numbers: List[Option[Double]]): List[String] = {
    numbers match {
      case Nil => Nil
      case head :: tail =>
        val res = head match {
          case Some(number) => "Результат деления = " + number
          case None => "Деление на ноль невозможно"
        }
        res :: doubleListToStr(tail)
    }
  }

  case class divPair(divisible: Int, divider: Int) {
    def getResult: Option[Double] = {
      divider match {
        case 0 => None
        case _ => Some(divisible.toDouble / divider)
      }
    }
  }

  def caseDivision(numbers: List[divPair]): List[Option[Double]] = {
    numbers match {
      case Nil => Nil
      case head :: tail =>
        head.getResult :: caseDivision(tail)
    }
  }

  abstract class Param {
    def getData: String
  }
  case class Val(data: Int) extends Param {
    override def getData: String = data.toString
  }
  case class Op(op: Char, param1: Param, param2: Param) extends Param {
    override def getData: String = param1.getData + param2.getData + op.toString
  }

//  def exprToRPN(str: String): String = {
//    val exprAsStr = str.toList
//
//    def exprToList(expr: List[Char], acc: String = ""): List[Any] = {
//      expr match {
//        case Nil => acc.toInt :: Nil
//        case head :: tail =>
//          head match {
//            case _ if Character.isDigit(head) =>
////              println(head)
//              exprToList(tail, acc + head)
//            case _ if ("+-*/()^").toList.contains(head) =>
////              println(head)
//              if (acc != "")
//                acc.toInt :: head :: exprToList(tail)
//              else
//                head :: exprToList(tail)
//            case _ =>
////              println("impossible character")
//              Nil
//          }
//      }
//    }
//
//    val expr = exprToList(exprAsStr)
//
//    def RPN(expr: List[Any], str: String = "", stack: List[Any] = Nil): String = {
////      println("---")
////      println(expr)
//      println(str)
////      println(stack)
//      expr match {
//        case Nil =>
//          if(stack != Nil)
//            RPN(expr, str + stack.head, stack.tail)
//          else
//            str
//        case head :: tail =>
//          head match {
//            case _: Int => RPN(tail, str + head.toString, stack)
//            case _ if List('+', '-').contains(head) =>
//              if ((stack != Nil) && List('+', '-', '*', '/', '^').contains(stack.head))
//                RPN(expr, str + stack.head, stack.tail)
//              else
//                RPN(tail, str, head :: stack)
//            case _ if List('*', '/').contains(head) =>
//              if ((stack != Nil) && List('*', '/', '^').contains(stack.head))
//                RPN(expr, str + stack.head, stack.tail)
//              else
//                RPN(tail, str, head :: stack)
//            case '^' =>
//              if ((stack != Nil) && stack.head == '^')
//                RPN(expr, str + stack.head, stack.tail)
//              else
//                RPN(tail, str, head :: stack)
//            case '(' =>
//              RPN(tail, str, head :: stack)
//            case ')' =>
//              if ((stack != Nil) && stack.head != '(')
//                RPN(expr, str + stack.head, stack.tail)
//              else
//                RPN(tail, str, stack.tail)
//
//          }
//      }
//    }
//
//    RPN(expr)
//  }


  def main (args: Array[String]): Unit = {
//    val lst: List[(Int, Int)] = List((6, 3), (3, 2), (3, 0), (10, 4))
//
//    val dList = division(lst)
//    println(dList)
//    println(doubleListToStr(dList))
//
//    val caseList: List[divPair] = List(divPair(6, 3), divPair(3, 2), divPair(3, 0), divPair(10, 4))
//    println(caseDivision(caseList))
    val expr = "3+4*2/(1-5)^2" // res = "342*15-2^/+"
    val exprTree = Op(
      '+',
      Val(3),
      Op(
        '/',
        Op(
          '*',
          Val(4),
          Val(2)
        ),
        Op(
          '^',
          Op(
            '-',
            Val(1),
            Val(5)
          ),
          Val(2)
        )
      )
    )
//    exprToRPN(expr)
    println(exprTree.getData)
  }
}
