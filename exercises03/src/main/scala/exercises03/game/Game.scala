package exercises03.game

object Game {
  def parseState(input: String, number: Int): State = input.toIntOption match {
    case Some(value) => {
      if (value < number) {
        NumberIsBigger
      } else if (value > number) {
        NumberIsSmaller
      } else {
        Guessed
      }
    }
    case None => {
      if (input == GameController.IGiveUp) {
        GiveUp
      } else {
        WrongInput
      }
    }
  }

  def action(state: State, number: Int): GameController => Unit = state match {
    case GiveUp          => GameController => GameController.giveUp(number)
    case WrongInput      => GameController => GameController.wrongInput()
    case NumberIsBigger  => GameController => GameController.numberIsBigger()
    case NumberIsSmaller => GameController => GameController.numberIsSmaller()
    case Guessed         => GameController => GameController.guessed()
  }

  def completed(state: State): Boolean = state == GiveUp || state == Guessed
}
