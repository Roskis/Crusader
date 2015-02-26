package game

import scala.io.Source
import org.lwjgl.opengl.Display
import Output.{startDisplay, drawMainMenu, drawGame, drawCharacterCreation}
import GameState.{MAIN_MENU, GAME, CHARACTER_CREATION}
import org.lwjgl.input.{Mouse, Keyboard}
import scala.util.Random
import collection.mutable.Buffer
import Direction._

/** Main object is responsible for the allocation of tasks to other parts of the program and 
 *  running the mainloop.
 */
object Main {
  
  private val rnd = new Random
  private var gameState: GameState.Value = MAIN_MENU
  private var monsterList = Buffer[Monster]()
  private var passiveObjectList = Buffer[PassiveObject]()
  private var equipmentList = Buffer[Equipment]()
  private var consumableList = Buffer[Consumable]()
  private var scrollList = Buffer[Scroll]()
  private var gameLog = Buffer[String]()
  private var lastWheel: Int = 0
  
  private var frameRate: Int = 0
  private var height: Int = 0
  private var width: Int = 0
  private var version: String = "alpha 0.01"
  
  private var player: Player = null
  private var grid: Grid = null
  private var episode: Int = 1
  private var level: Int = 1
  private var turn: Int = 0
  
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
    player = new Player("Paladin", 0, 0)
    /** Gameloop */
    while (!Display.isCloseRequested) {
      gameState match {
        case g if (g == MAIN_MENU) => {
          menuKeys
          drawMainMenu
        }
        case g if (g == GAME) => {
          gameKeys
          drawGame
        }
        case g if (g == CHARACTER_CREATION) => {
          characterKeys
          drawCharacterCreation
        }
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
    monsterList.clear
    level = 0
    episode = 1
    turn = 0
    while (Keyboard.next) {}
    nextMap()
  }
  
  /** Advance one level */
  def nextMap() {
    clearLists
    level += 1
    grid = new Grid()
    grid.init
  }
  
  def characterKeys {
    if ((Mouse.isButtonDown(0) && Mouse.getX > 816 && Mouse.getX < 1040 && 
        (height - Mouse.getY) > 604 && (height - Mouse.getY) < 660)) {
          gameState = GAME
          while (Keyboard.next) {}
          newGame
    }
    else if ((Mouse.isButtonDown(0) && Mouse.getX > 274 && Mouse.getX < 496 && 
        (height - Mouse.getY) > 604 && (height - Mouse.getY) < 660)) {
      gameState = MAIN_MENU
      while (Keyboard.next) {}
    }
    else while (Keyboard.next) {
      Keyboard.getEventKey match {
        case k if (k == Keyboard.KEY_ESCAPE) => gameState = MAIN_MENU
        case k if (k == Keyboard.KEY_RETURN && Keyboard.getEventKeyState && player.name != "" && Output.font.getWidth(getPlayer.name) < 400) => {
          gameState = GAME
          while (Keyboard.next) {}
          newGame
        }
        case k if (k == Keyboard.KEY_BACK && Keyboard.getEventKeyState) => player.name = player.name.dropRight(1)
        case k if (Keyboard.getEventKeyState) => {
          if (k == 57 && Keyboard.getEventKeyState) player.name = player.name + " "
          else if ((k == 0 || k == 12 || k == 13 || k == 15 || k == 26 || k == 27 || k == 28 || k == 29 || k == 39 || 
              k == 40 || k == 41 || k == 42 || k == 43 || k > 50) && Keyboard.getEventKeyState) {}
          else if (player.name == "" && Keyboard.getEventKeyState) player.name = Keyboard.getKeyName(k).toUpperCase
          else player.name = player.name + Keyboard.getKeyName(k).toLowerCase
          }
        case _ => {}
      }
    }
  }
  
  /** Follow user input in menu */
  def menuKeys {
    if ((Mouse.isButtonDown(0) && Mouse.getX > 922 && Mouse.getX < 1145 && 
        (height - Mouse.getY) > 320 && (height - Mouse.getY) < 376) || 
        Keyboard.isKeyDown(Keyboard.KEY_N)) {
      gameState = CHARACTER_CREATION
      while (Keyboard.next) {}
      player = new Player(player.name, 0, 0)
    }
    else if ((Mouse.isButtonDown(0) && Mouse.getX > 922 && Mouse.getX < 1145 && 
        (height - Mouse.getY) > 420 && (height - Mouse.getY) < 476) || 
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
        case k if ((k == Keyboard.KEY_D || k == Keyboard.KEY_RIGHT || k == Keyboard.KEY_NUMPAD6) && 
            Keyboard.getEventKeyState && getPlayer.health > 0) => {
          player.moveOrAttack(E)
          playTurn
        }
        case k if ((k == Keyboard.KEY_A || k == Keyboard.KEY_LEFT || k == Keyboard.KEY_NUMPAD4) && 
            Keyboard.getEventKeyState && getPlayer.health > 0) => {
          player.moveOrAttack(W)
          playTurn
        }
        case k if ((k == Keyboard.KEY_W || k == Keyboard.KEY_UP || k == Keyboard.KEY_NUMPAD8) && 
            Keyboard.getEventKeyState && getPlayer.health > 0) => {
          player.moveOrAttack(N)
          playTurn
        }
        case k if ((k == Keyboard.KEY_S || k == Keyboard.KEY_DOWN || k == Keyboard.KEY_NUMPAD2) && 
            Keyboard.getEventKeyState && getPlayer.health > 0) => {
          player.moveOrAttack(S)
          playTurn
        }
        case k if ((k == Keyboard.KEY_Q || k == Keyboard.KEY_NUMPAD7) && 
            Keyboard.getEventKeyState && getPlayer.health > 0) => {
          player.moveOrAttack(NW)
          playTurn
        }
        case k if ((k == Keyboard.KEY_E || k == Keyboard.KEY_NUMPAD9) && 
            Keyboard.getEventKeyState && getPlayer.health > 0) => {
          player.moveOrAttack(NE)
          playTurn
        }
        case k if ((k == Keyboard.KEY_C || k == Keyboard.KEY_NUMPAD3) && 
            Keyboard.getEventKeyState && getPlayer.health > 0) => {
          player.moveOrAttack(SE)
          playTurn
        }
        case k if ((k == Keyboard.KEY_Z || k == Keyboard.KEY_NUMPAD1) && 
            Keyboard.getEventKeyState && getPlayer.health > 0) => {
          player.moveOrAttack(SW)
          playTurn
          }
        case _ => {}
      }
    }
    lastWheel = Mouse.getDWheel
  }
  
  /** Play one turn */
  def playTurn() {
    for (monster <- monsterList)
      monster.turn
    turn += 1
    if (player.health < 0) Output.addLog("You died! Score: " + getPlayer.gold)
  }
  
  def clearLists() {
    monsterList.clear
    passiveObjectList.clear
    equipmentList.clear
    consumableList.clear
    scrollList.clear
  }
  
  /** Some getters */
  def getRnd() = rnd
  def getGameState() = gameState
  def getMonsterList() = monsterList
  def getPassiveObjectList() = passiveObjectList
  def getEquipmentList() = equipmentList
  def getConsumableList() = consumableList
  def getScrollList() = scrollList
  def getGameLog() = gameLog
  def getFrameRate() = frameRate
  def getHeight() = height
  def getWidth() = width
  def getVersion() = version
  def getPlayer() = player
  def getGrid() = grid
  def getEpisode() = episode
  def getLevel() = level
  def getTurn() = turn
  def getLastWheel() = lastWheel
  
}