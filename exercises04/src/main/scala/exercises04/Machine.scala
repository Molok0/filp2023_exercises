package exercises04

case class Machine(locked: Boolean, candies: Int, coins: Int)

/**
  * Реализуйте вендинговый аппарат по торговле барбарисками. Правила работы аппарата следующие:
  * если в закрытый аппарат вставляется монета (Coin), то аппартат открывается
  * если повернуть ручку (Turn) у открытого аппарата, то выйдет барбариска, и аппарат закроется
  * если в аппарате кончились барбариски, то он никак не реагирует. в этом случае надо вернуть список оставшихся Inputs и закончить
  * другие действия приводят к пропуску Input
  * если Input кончился, то заканчиваем
  * Подразумевается, что вы будете использовать паттерн-матчинг и рекурсию, так как while var isInstanceOf запрещены.
  */
object Machine {
  sealed trait Input
  object Input {
    case object Coin extends Input
    case object Turn extends Input
  }

  @scala.annotation.tailrec
  def run(machine: Machine, inputs: List[Input]): (Machine, List[Input]) = {

    if (machine.candies == 0) {
      (machine, inputs)
    } else {
      inputs match {
        case ::(head, next) =>
          head match {
            case Input.Coin => {
              if (machine.locked == false) run(Machine(false, machine.candies, machine.coins), next)
              else run(Machine(false, machine.candies, machine.coins + 1), next)
            }
            case Input.Turn => {
              if (machine.locked == true) run(Machine(true, machine.candies, machine.coins), next)
              else run(Machine(true, machine.candies - 1, machine.coins), next)
            }
          }
        case Nil => (machine, Nil)
      }
    }
  }
}
