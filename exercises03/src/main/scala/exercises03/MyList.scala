package exercises03

import scala.annotation.tailrec
import scala.collection.IndexedSeqView.Reverse

sealed trait MyList[+A]

final case class Cons[A](head: A, tail: MyList[A]) extends MyList[A]

case object Nil extends MyList[Nothing]

object MyList {
  def sum(list: MyList[Int]): Int = {
    list match {
      case Cons(head, tail) => head + sum(tail)
      case Nil              => 0
    }
  }

  def reverse[A](list: MyList[A]): MyList[A] = {
    @tailrec
    def reverseRec(list: MyList[A], reverseList: MyList[A]): MyList[A] = {
      list match {
        case Cons(head, tail) => reverseRec(tail, Cons(head, reverseList))
        case Nil              => reverseList
      }
    }
    reverseRec(list, Nil)
  }

}
