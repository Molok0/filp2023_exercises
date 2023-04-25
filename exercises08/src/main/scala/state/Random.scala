package state

import typeclasses.Monad

import scala.annotation.tailrec

object StatefulRandom {

  import scala.util.Random

  // Если вам необходимо сгенерить рандомное число, то вы можете использовать класс scala.util.Random.
  println(Random.nextDouble)
  println(Random.nextInt)
  println(Random.nextInt)

  // Каждый вызов возвращает новое число. Можно предположить, что rnd имеет какое-то внутреннее
  // состояние. Когда происходит очередной вызов nextInt/nextDouble, то rnd не только возвращает
  // новое случайное число, но изменяет внутреннее состояние.
  //
  // Такое скрытое изменение внутреннего состояние имеет ряд недостатков: программы тяжелее
  // композировать, тестировать, усложняется написание многопоточных программ.
}

object StatelessRandom {

  // Чтобы избавиться от скрытого изменяемого состояния, давайте сделаем его явным. Новая функция
  // nextInt возвращает не только случайное число, но и новое состояние генератора случайных чисел.
  trait Random {
    def nextInt: (Int, Random)
  }

  // В качесте простой реализации генератора случайных чисел будем использовать следующий класс
  case class SimpleRandom(seed: Long) extends Random {
    override def nextInt: (Int, Random) = {
      val newSeed = 13 * seed + 41
      val int     = (newSeed >>> 3).toInt
      (int, SimpleRandom(newSeed))
    }
  }

  // и простой пример использования
  val rnd: SimpleRandom     = SimpleRandom(12)
  val (firstInt, nextRnd1)  = rnd.nextInt
  val (secondInt, nextRnd2) = nextRnd1.nextInt
  val (thirdInt, nextRnd3)  = nextRnd2.nextInt
  // Вместо скрытого изменения состояния, мы теперь делаем все изменения явными. Чтобы получать
  // каждый раз разные значения, мы вызываем nextInt на разных объектах: rnd, nextRnd1, nextRnd2 и
  // так далее.

  // Реализуйте функцию, которая будет генерировать пару случайных целых чисел
  def pair(rnd: Random): ((Int, Int), Random) = {
    val (firstInt, nextRnd1)  = rnd.nextInt
    val (secondInt, nextRnd2) = nextRnd1.nextInt
    ((firstInt, secondInt), nextRnd2)
  }

  // Функцию, которая генерирует неотрицальные числа
  def nonNegativeInt(rnd: Random): (Int, Random) = {
    val (n, nextRnd) = rnd.nextInt
    (if (n < 0) -(n + 1) else n, nextRnd)
  }

  // Функцию, которая генерирует случайное число от нуля включительно до единицы невключительно
  @tailrec
  def double(rnd: Random): (Double, Random) = {
    val (n, nextRnd) = nonNegativeInt(rnd)
    if (n >= (1 << 27)) double(nextRnd) else (n.toDouble / (1 << 27), nextRnd)
  }
}

// Нам удалось избавиться от скрытого изменяемого состояния, но приходится передавать теперь его явно.
// Это приводит к написанию достаточно однообразного кода. Давайте попробуем немного модифицировать
// подход, чтобы избавиться от этого.
object BetterStatelessRandom {

  import typeclasses.Monad.syntax._
  import StatelessRandom.Random

  // Можно заметить, что наши функции pair, nonNegativeInt, double имеют одинаковый шаблон
  // Rnd => (A, Rnd), где тип A зависит от конкретной функции. Можно сказать, что эти функции
  // описывают переход состояния. Давайте обобщим этот шаблон в виде класса RandomState.
  case class RandomState[A](run: Random => (A, Random))

  object RandomState {
    implicit val monad: Monad[RandomState[*]] = new Monad[RandomState[*]] {

      def pure[A](a: A): RandomState[A] =
        RandomState(s => (a, s))

      override def map[A, B](fa: RandomState[A])(f: A => B): RandomState[B] =
        RandomState(rnd =>
          fa.run(rnd) match {
            case (a, random) => (f(a), random)
          }
        )

      override def flatMap[A, B](fa: RandomState[A])(f: A => RandomState[B]): RandomState[B] =
        RandomState(s =>
          fa.run(s) match {
            case (value, nextState) => f(value).run(nextState)
          }
        )
    }
  }

  // Теперь класс RandomState может быть использован внутри for comprehension

  // Функция возвращает случайное целое число
  val nextInt: RandomState[Int] = RandomState(_.nextInt)

  // Функция возвращает случайное неотрицальное целое число
  val nonNegativeInt: RandomState[Int] = RandomState(_.nextInt match {
    case (n, r) if n < 0 => (-(n + 1), r)
    case (n, r)          => (n, r)
  })

  // Функция возвращает пару случайных неотрицальных целых чисел
  val pair: RandomState[(Int, Int)] = for {
    x <- nonNegativeInt
    y <- nonNegativeInt
  } yield (x, y)

  // Функция возвращает случайное число от нуля до единицы
  val double: RandomState[Double] = RandomState { rnd =>
    val (n, nextRnd) = StatelessRandom.nonNegativeInt(rnd)
    if (n >= (1 << 27)) double.run(nextRnd) else (n.toDouble / (1 << 27), nextRnd)
  }

  // Функция возвращает список случайной длины из случайных целых чисел
  val randomList: RandomState[List[Int]] = {
    for {
      length <- nonNegativeInt.map(_ % 10)
      list   <- sequence(List.fill(length)(nextInt))
    } yield list.sorted.reverse
  }

  // Функция должна сконвертировать список из случайных состояний в случайное состояние, которое
  // возвращает список.
  def sequence[A](xs: List[RandomState[A]]): RandomState[List[A]] = xs.foldRight(RandomState(List.empty[A] -> _)) {
    (elem, acc) =>
      for {
        x  <- elem
        xs <- acc
      } yield (x :: xs).reverse
  }
}
