package exercises04.calculator

import scala.Integral.Implicits.infixIntegralOps

// Необходимо реализовать функцию сalculate для вычисления выражений
class Calculator[T: Integral] {
  def isZero(t: T): Boolean =
    t == implicitly[Integral[T]].zero

  def calculate(expr: Expr[T]): Result[T] = {
    expr match {
      case Mul(left, right) =>
        (calculate(left), calculate(right)) match {
          case (Success(a), Success(b)) => Success(a * b)
          case _                        => DivisionByZero
        }
      case Div(left, right) =>
        (calculate(left), calculate(right)) match {
          case (Success(a), Success(b)) =>
            if (b != 0) Success(a / b)
            else DivisionByZero
          case _ => DivisionByZero
        }
      case Plus(left, right) =>
        (calculate(left), calculate(right)) match {
          case (Success(a), Success(b)) => Success(a + b)
          case _                        => DivisionByZero
        }
      case Minus(left, right) =>
        (calculate(left), calculate(right)) match {
          case (Success(a), Success(b)) => Success(a - b)
          case _                        => DivisionByZero
        }
      case Val(v) => Success(v)
      case If(iff, cond, left, right) =>
        calculate(cond) match {
          case Success(value) =>
            if (iff(value)) calculate(left)
            else calculate(right)
          case _ => DivisionByZero
        }
    }
  }
}
