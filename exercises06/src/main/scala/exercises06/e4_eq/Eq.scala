package exercises06.e4_eq

trait Eq[A] {
  def eqv(a: A, b: A): Boolean
}

object Eq {
  def apply[A](implicit ev: Eq[A]): Eq[A] = ev
}

object EqInstances {
  implicit val intEq: Eq[Int] = _ == _

  implicit val boolEq: Eq[Boolean] = _ == _

  implicit def optionEq[A: Eq]: Eq[Option[A]] =
    (a: Option[A], b: Option[A]) =>
      (a, b) match {
        case (None, None)       => true
        case (Some(x), Some(y)) => Eq[A].eqv(x, y)
        case _                  => false
      }

  implicit def listEq[A: Eq]: Eq[List[A]] =
    (a: List[A], b: List[A]) => a.length == b.length && a.zip(b).forall { case (a, b) => Eq[A].eqv(a, b) }

}

object EqSyntax {

  implicit class EqOps[A](val a: A) extends AnyVal {
    def eqv(b: A)(implicit ev: Eq[A]): Boolean = ev.eqv(a, b)

    def ===(b: A)(implicit ev: Eq[A]): Boolean = eqv(b)

    def !==(b: A)(implicit ev: Eq[A]): Boolean = !eqv(b)
  }
}

object Examples {
  import EqInstances._
  import EqSyntax._

  1 eqv 1 // возвращает true
  1 === 2 // возвращает false
  1 !== 2 // возвращает true
//  1 === "some-string" // не компилируется
//  1 !== Some(2) // не компилируется
  List(true) === List(true) // возвращает true
}
