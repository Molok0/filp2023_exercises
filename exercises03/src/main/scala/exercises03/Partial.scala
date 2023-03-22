package exercises03

object Partial {
  def combo[I, T](funcs: List[PartialFunction[I, T]]): I => Option[T] = { i: I =>
    funcs.foldLeft[Option[T]](Option.empty[T])((acc, f) => acc.orElse(f.lift(i)))
  }
}
