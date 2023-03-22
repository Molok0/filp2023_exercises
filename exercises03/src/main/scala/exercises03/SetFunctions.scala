package exercises03

object SetFunctions {
  type Set[A] = A => Boolean

  def contains[A](s: Set[A], elem: A): Boolean = s(elem)

  def singletonSet[A](elem: A): Set[A] = Set(elem)

  def union[A](s: Set[A], t: Set[A]): Set[A] = i => s(i) || t(i)

  def intersect[A](s: Set[A], t: Set[A]): Set[A] = i => s(i) && t(i)

  def diff[A](s: Set[A], t: Set[A]): Set[A] = i => !(s(i) && t(i))

  def symmetricDiff[A](s: Set[A], t: Set[A]): Set[A] = i => (s(i) || t(i)) && !((s(i) && t(i)))

  def filter[A](s: Set[A], p: A => Boolean): Set[A] = i => p(i)

  def cartesianProduct[A, B](as: Set[A], bs: Set[B]): Set[(A, B)] = {
    case (a, b) => contains(as, a) && contains(bs, b)
  }
}
