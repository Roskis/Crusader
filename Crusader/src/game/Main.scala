package game

import scala.io.Source
import org.lwjgl.opengl.Display
import Output.{startDisplay, drawMainMenu, drawGame}
import GameState.{MAIN_MENU, GAME}
import org.lwjgl.input.{Mouse, Keyboard}
import scala.util.Random
import collection.mutable.Buffer

/** Main object is responsible for the allocation of tasks to other parts of the program and 
 *  running the mainloop.
 */
object Main {
  
  val rnd = new Random
  var gameState: GameState.Value = MAIN_MENU
  var monsterList = Buffer[Monster]()
  
  var frameRate: Int = 0
  var height: Int = 0
  var width: Int = 0
  var version: String = "alpha 0.01"
  
  var player: Player = null
  var grid: Grid = null
  var episode: Int = 1
  var level: Int = 0
  var turn: Int = 0
  
  /** Loads some data from init-file */
  val initFile = Source.fromFile("src/init.txt")
  try {
    for (row <- initFile.getLines().toVector) {
      if (row.split("=")(0).trim.toUpperCase == "FRAMERATE") frameRate = row.split("=")(1).trim.toString.toInt
      else if (row.split("=")(0).trim.toUpperCase == "HEIGHT") height = row.split("=")(1).trim.toString.toInt
      else if (row.split("=")(0).trim.toUpperCase == "WIDTH") width = row.split("=")(1).trim.toString.toInt
      else if (row.split("=")(0).trim.toUpperCase == "VERSION") version = row.split("=")(1).trim.toString
    }
  } finally initFile.close
  
  /** The program's main method.
    *
    * The program will start here. Mainloop includes the creation for window, processing the user 
    * input and asking Output to draw window.
    */
  def main(args:Array[String]) {
    startDisplay()
    /** Gameloop */
    while (!Display.isCloseRequested) {
      gameState match {
        case g if (g == MAIN_MENU) => menuKeys
        case g if (g == GAME) => gameKeys
      }
      gameState match {
        case g if (g == MAIN_MENU) => drawMainMenu
        case g if (g == GAME) => drawGame
      }
      Display.update()
      Display.sync(frameRate)
    }
    /** Since gameloop has ended display will be removed */
    Display.destroy()
    System.exit(0)
    
  }
  
  /** Start new game */
  def newGame() {
    makeTestMap()
    val playerPosition = grid.giveRandomNonBlockinCoordinates
    player = new Player("Paladin", playerPosition._1, playerPosition._2)
    episode = 1
    level = 1
    turn = 0
    while (Keyboard.next) {}
  }
  
  /** Initialize test map */
  def makeTestMap() {
    grid = new Grid(rnd.nextInt(5)+30)
    monsterList.clear()
    for (n <- Range(0, 3)) {
      var lizardPosition = grid.giveRandomNonBlockinCoordinates
      var lizard = new Monster("Lizard", "Nasty looking reptile.", 
          lizardPosition._1, lizardPosition._2, "Monsters/ep1/lizarda1")
      monsterList.append(lizard)
    }
  }
  
  /** Advance one level */
  def nextTestMap() {
    grid = new Grid(rnd.nextInt(5)+30)
    val playerPosition = grid.giveRandomNonBlockinCoordinates
    player.setX(playerPosition._1)
    player.setY(playerPosition._2)
    for (n <- Range(0,monsterList.size)) {
      var lizardPosition = grid.giveRandomNonBlockinCoordinates
      var lizard = new Monster("Lizard", "Nasty looking reptile.", 
          lizardPosition._1, lizardPosition._2, "Monsters/ep1/lizarda1")
      monsterList.append(lizard)
    }
    level += 1
  }
  
  /** Follow user input in gamemenu */
  def menuKeys {
    if ((Mouse.isButtonDown(0) && Mouse.getX >= 910 && Mouse.getX <= 1158 && 
        (height - Mouse.getY) >= 324 && (height - Mouse.getY) <= 372) || 
        Keyboard.isKeyDown(Keyboard.KEY_N)) {
      gameState = GAME
      newGame
    }
    else if ((Mouse.isButtonDown(0) && Mouse.getX >= 910 && Mouse.getX <= 1158 && 
        (height - Mouse.getY) >= 424 && (height - Mouse.getY) <= 472) || 
        Keyboard.isKeyDown(Keyboard.KEY_Q)) {
      Display.destroy()
      System.exit(0)
    }
  }
  
  /** Follow user input in game */
  def gameKeys {
    while (Keyboard.next) {
      Keyboard.getEventKey match {
        case k if (k == Keyboard.KEY_ESCAPE) => gameState = MAIN_MENU
        case k if ((k == Keyboard.KEY_D || Keyboard.getEventKey == Keyboard.KEY_RIGHT) && Keyboard.getEventKeyState) => {
          player.moveOrAttack(player.getX + 1, player.getY)
          playTurn
        }
        case k if ((k == Keyboard.KEY_A || Keyboard.getEventKey == Keyboard.KEY_LEFT) && Keyboard.getEventKeyState) => {
          player.moveOrAttack(player.getX - 1, player.getY)
          playTurn
        }
        case k if ((k == Keyboard.KEY_W || Keyboard.getEventKey == Keyboard.KEY_UP) && Keyboard.getEventKeyState) => {
          player.moveOrAttack(player.getX, player.getY - 1)
          playTurn
        }
        case k if ((k == Keyboard.KEY_S || Keyboard.getEventKey == Keyboard.KEY_DOWN) && Keyboard.getEventKeyState) => {
          player.moveOrAttack(player.getX, player.getY + 1)
          playTurn
        }
        case _ => {}
      }
    }
  }
  
  /** Play one turn */
  def playTurn() {
    for (monster <- monsterList)
      monster.turn
    turn += 1
  }
}