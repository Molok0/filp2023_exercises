package exercises05.either

object EitherCombinators {

  sealed trait Either[+A, +B] {
    def orElse[EE >: A, C >: B](other: => Either[EE, C]): Either[EE, C] = this match {
      case Left(get) =>
        other match {
          case Left(x)  => Left(get)
          case Right(x) => Right(x)
        }
      case Right(get) => Right(get)
    }

    def flatMap[AA >: A, BB](f: B => Either[AA, BB]): Either[AA, BB] = this match {
      case Left(get)  => Left(get)
      case Right(get) => f(get)
    }

    def map[C](f: B => C): Either[A, C] = this match {
      case Left(get)  => Left(get)
      case Right(get) => Right(f(get))
    }
    def map2[AA >: A, BB, C](other: => Either[AA, BB])(f: (B, BB) => C): Either[AA, C] = {
      this match {
        case Left(get) => Left(get)
        case Right(get) =>
          other match {
            case Left(x)  => Left(x)
            case Right(x) => Right(f(get, x))
          }
      }
    }

  }

  case class Left[+A, +B](get: A) extends Either[A, B]

  case class Right[+A, +B](get: B) extends Either[A, B]

  object Either {
    def fromOption[A, B](option: Option[B])(a: => A): Either[A, B] = option match {
      case Some(value) => Right(value)
      case None        => Left(a)
    }

    def traverse[E, A, B](list: List[A])(f: A => Either[E, B]): Either[E, List[B]] = {
      def loop(lst: List[A], acc: List[B]): Either[E, List[B]] = lst match {
        case Nil => Right(acc.reverse)
        case head :: tail =>
          f(head) match {
            case Left(x)  => Left(x)
            case Right(x) => loop(tail, x :: acc)
          }
      }
      loop(list, Nil)
    }

    def sequence[E, A](list: List[Either[E, A]]): Either[E, List[A]] = {
      def loop[E, A](lst: List[Either[E, A]], acc: List[A]): Either[E, List[A]] = lst match {
        case ::(head, tail) =>
          head match {
            case Left(get)  => Left(get)
            case Right(get) => loop(tail, get :: acc)
          }
        case Nil => Right(acc.reverse)
      }
      loop(list, Nil)
    }

  }
}
