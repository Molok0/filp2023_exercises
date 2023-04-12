package exercises07.ex01

import exercises07.data.NonEmptyList
import exercises07.typeclasses._

object Exercise01 {
  object Syntax {

    implicit class SOps[A](private val a: A) extends AnyVal {
      def |+|(b: A)(implicit semigroup: Semigroup[A]): A =
        semigroup.combine(a, b)

      def pure[F[_]: Applicative]: F[A] = Applicative[F].pure(a)
    }

    implicit class AppOps[F[_], A](private val fa: F[A]) extends AnyVal {

      def aproduct[B](fb: F[B])(implicit ap: Applicative[F]): F[(A, B)] = ap.product(fa, fb)

      def ap[B](ff: F[A => B])(implicit ap: Applicative[F]): F[B] = Applicative[F].ap(ff)(fa)

      def traverse[G[_]: Applicative, B](f: A => G[B])(implicit traverse: Traverse[F]): G[F[B]] =
        traverse.traverse(fa)(f)

      def foldLeft[B](b: B)(ff: (B, A) => B)(implicit foldable: Foldable[F]): B = foldable.foldLeft(fa, b)(ff)

      def map[B](f: A => B)(implicit functor: Functor[F]): F[B] = functor.map(fa)(f)

      def combineAll(implicit foldable: Foldable[F], monoid: Monoid[A]): A =
        foldable.foldLeft(fa, monoid.empty)(monoid.combine)

    }
  }

  object Instances {

    import Syntax._

    implicit val strMonoid = new Monoid[String] {
      def empty: String = ""

      def combine(x: String, y: String): String = x + y
    }

    implicit val intMonoid = new Monoid[Int] {
      def empty: Int = 0

      def combine(x: Int, y: Int): Int = x + y
    }

    implicit val listInstances: Traverse[List] with Applicative[List] = new Traverse[List] with Applicative[List] {

      def traverse[G[_]: Applicative, A, B](fa: List[A])(f: A => G[B]): G[List[B]] = {
        fa.foldLeft(List.empty[B].pure[G])((accF, next) =>
          accF.aproduct(f(next)).map({ case (acc, next) => acc.appended(next) })
        )
      }

      def ap[A, B](ff: List[A => B])(fa: List[A]): List[B] = ff.zip(fa).map { case (a, b) => a(b) }

      def pure[A](x: A): List[A] = List(x)

      def map[A, B](fa: List[A])(f: A => B): List[B] =
        fa.map(f)

      def foldLeft[A, B](fa: List[A], b: B)(f: (B, A) => B): B =
        fa.foldLeft(b)(f)
    }

    implicit val optionInstances: Traverse[Option] with Applicative[Option] =
      new Traverse[Option] with Applicative[Option] {

        def traverse[G[_]: Applicative, A, B](fa: Option[A])(f: A => G[B]): G[Option[B]] = fa match {
          case Some(x) => f(x).map(Some(_))
          case None    => Option.empty[B].pure[G]
        }

        def ap[A, B](ff: Option[A => B])(fa: Option[A]): Option[B] = ff match {
          case Some(f) =>
            fa match {
              case Some(x) => Some(f(x))
              case None    => Option.empty[B]
            }
          case None => Option.empty[B]
        }

        def pure[A](x: A): Option[A] =
          Some(x)

        def map[A, B](fa: Option[A])(f: A => B): Option[B] =
          fa match {
            case None    => None
            case Some(x) => Some(f(x))
          }

        def foldLeft[A, B](fa: Option[A], b: B)(f: (B, A) => B): B = fa match {
          case Some(x) => f(b, x)
          case None    => b
        }
      }

    implicit val nelInstances: Traverse[NonEmptyList] with Applicative[NonEmptyList] =
      new Traverse[NonEmptyList] with Applicative[NonEmptyList] {
        def traverse[G[_]: Applicative, A, B](fa: NonEmptyList[A])(f: A => G[B]): G[NonEmptyList[B]] = {
          f(fa.head).aproduct(fa.tail.traverse(f)).map { case (a, b) => NonEmptyList(a, b) }
        }

        def ap[A, B](ff: NonEmptyList[A => B])(fa: NonEmptyList[A]): NonEmptyList[B] =
          NonEmptyList(ff.head(fa.head), fa.tail.ap(ff.tail))

        def pure[A](x: A): NonEmptyList[A] = NonEmptyList(x)

        def foldLeft[A, B](fa: NonEmptyList[A], b: B)(f: (B, A) => B): B = fa.tail.foldLeft(f(b, fa.head))(f)

        def map[A, B](fa: NonEmptyList[A])(f: A => B): NonEmptyList[B] = NonEmptyList(f(fa.head), fa.tail.map(f))

      }

    implicit def listMonoid[A] = new Monoid[List[A]] {
      def empty: List[A] = List.empty[A]

      def combine(x: List[A], y: List[A]): List[A] = x ::: y
    }
  }
}
