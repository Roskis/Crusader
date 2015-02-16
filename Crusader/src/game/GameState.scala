package game

/** There is two states of game */
object GameState extends Enumeration {

  type GameState = Value
  val GAME, MAIN_MENU = Value

  def change(g: GameState): GameState = {
    g match {
      case GAME => GAME
      case MAIN_MENU => MAIN_MENU
    }
  }

}