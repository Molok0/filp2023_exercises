package exercises05

object Combinators {
  // Есть цепочка hefEgGeGFEgGgeHE
  // в данной цепочке есть различные типы частиц
  // f, e, h, g положительно заряженные частицы
  // F, E, H, G отрицательно заряженные частицы
  // если частицы одного типа с разной полярностью стоят вместе в цепочке, они реагируют и исчезают
  // проход слева направо
  //
  // hefEgGeGFEgGgeHE <- gG прореагировали
  // hefEeGFEgGgeHE <- Ee прореагировали
  // hefGFEgGgeHE <- gG
  // hefGFEgeHE <- итоговая цепочка, в которой 10 частиц
  //
  // Напишите функцию, используя комбинаторы стандартной библиотеки,
  // которая проведёт полную реакцию
  def react(ipt: String): String = {
    val result = ipt.foldLeft(List.empty[Char]) { (acc, c) =>
      acc match {
        case x :: xs if check(x, c) => xs
        case _                      => c :: acc
      }
    }
    result.reverse.mkString
  }

  def check(c1: Char, c2: Char): Boolean = c1.toLower == c2.toLower && c1 != c2

}
