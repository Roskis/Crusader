package game

/** There are two states of game */
object GameState extends Enumeration {

  type GameState = Value
  val GAME, MAIN_MENU = Value

}