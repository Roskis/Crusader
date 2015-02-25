package game

/** There are three states of game */
object GameState extends Enumeration {

  type GameState = Value
  val GAME, MAIN_MENU, CHARACTER_CREATION = Value

}