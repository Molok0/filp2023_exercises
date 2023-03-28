package exercises04.calculator

import scala.Integral.Implicits.infixIntegralOps

// Необходимо реализовать функцию сalculate для вычисления выражений
class Calculator[T: Integral] {
  def isZero(t: T): Boolean =
    t == implicitly[Integral[T]].zero

  def resultOperation(exprLeft: Expr[T], exprRight: Expr[T])(op: (T, T) => T): Result[T] = {
    (calculate(exprLeft), calculate(exprRight)) match {
      case (Success(a), Success(b)) => Success(op(a, b))
      case _                        => DivisionByZero
    }
  }
  def calculate(expr: Expr[T]): Result[T] = {
    expr match {
      case Mul(left, right) => resultOperation(left, right)(_ * _)
      case Div(left, right) =>
        (calculate(left), calculate(right)) match {
          case (Success(a), Success(b)) =>
            if (b != 0) Success(a / b)
            else DivisionByZero
          case _ => DivisionByZero
        }
      case Plus(left, right)  => resultOperation(left, right)(_ + _)
      case Minus(left, right) => resultOperation(left, right)(_ - _)
      case Val(v)             => Success(v)
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
