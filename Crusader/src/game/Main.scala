package game

import scala.collection.mutable.Buffer
import scala.io.Source
import scala.util.Random

import org.lwjgl.input.Keyboard
import org.lwjgl.input.Mouse
import org.lwjgl.opengl.Display

import Direction.E
import Direction.N
import Direction.NE
import Direction.NW
import Direction.S
import Direction.SE
import Direction.SW
import Direction.W
import GameState.CHARACTER_CREATION
import GameState.GAME
import GameState.LEVEL
import GameState.MAIN_MENU
import Output.drawCharacterCreation
import Output.drawGame
import Output.drawMainMenu
import Output.startDisplay

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
  private var prevMouseState: Boolean = false
  
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
        case g if (g == LEVEL) => {
          levelKeys
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
  
  /** Follow user input when choosing new level */
  def levelKeys {
    if (Mouse.isButtonDown(0) && Mouse.getX > 149 && Mouse.getX < 406 && !prevMouseState && 
        (height - Mouse.getY) > 606 && (height - Mouse.getY) < 671) {
      gameState = GAME
      while (Keyboard.next) {}
    }
    else if (player.health > 0) {
      if (Mouse.isButtonDown(0) && player.experience >= player.xpNeededForLevel(player.zeal) && !prevMouseState && 
          Mouse.getX > 149 && Mouse.getX < 406 && (height - Mouse.getY) > 117 && (height - Mouse.getY) < 181) {
        player.experience -= player.xpNeededForLevel(player.zeal)
        player.zeal += 1
      }
      else if (Mouse.isButtonDown(0) && player.experience >= player.xpNeededForLevel(player.humility) && !prevMouseState && 
          Mouse.getX > 149 && Mouse.getX < 406 && (height - Mouse.getY) > 187 && (height - Mouse.getY) < 251) {
        player.experience -= player.xpNeededForLevel(player.humility)
        player.humility += 1
      }
      else if (Mouse.isButtonDown(0) && player.experience >= player.xpNeededForLevel(player.temperance) && !prevMouseState && 
          Mouse.getX > 149 && Mouse.getX < 406 && (height - Mouse.getY) > 257 && (height - Mouse.getY) < 321) {
        player.experience -= player.xpNeededForLevel(player.temperance)
        player.temperance += 1
      }
      else if (Mouse.isButtonDown(0) && player.experience >= player.xpNeededForLevel(player.kindness) && !prevMouseState && 
          Mouse.getX > 149 && Mouse.getX < 406 && (height - Mouse.getY) > 327 && (height - Mouse.getY) < 391) {
        player.experience -= player.xpNeededForLevel(player.kindness)
        player.kindness += 1
      }
      else if (Mouse.isButtonDown(0) && player.experience >= player.xpNeededForLevel(player.patience) && !prevMouseState && 
          Mouse.getX > 149 && Mouse.getX < 406 && (height - Mouse.getY) > 397 && (height - Mouse.getY) < 461) {
        player.experience -= player.xpNeededForLevel(player.patience)
        player.patience += 1
      }
      else if (Mouse.isButtonDown(0) && player.experience >= player.xpNeededForLevel(player.charity) && !prevMouseState && 
          Mouse.getX > 149 && Mouse.getX < 406 && (height - Mouse.getY) > 467 && (height - Mouse.getY) < 531) {
        player.experience -= player.xpNeededForLevel(player.charity)
        player.charity += 1
      }
      else if (Mouse.isButtonDown(0) && player.experience >= player.xpNeededForLevel(player.diligence) && !prevMouseState && 
          Mouse.getX > 149 && Mouse.getX < 406 && (height - Mouse.getY) > 537 && (height - Mouse.getY) < 601) {
        player.experience -= player.xpNeededForLevel(player.diligence)
        player.diligence += 1
      }
    }
    prevMouseState = Mouse.isButtonDown(0)
  }
  
  /** Follow user input when creating new character */
  def characterKeys {
    if ((Mouse.isButtonDown(0) && Mouse.getX > 799 && Mouse.getX < 1056 && 
        (height - Mouse.getY) > 600 && (height - Mouse.getY) < 665)) {
          gameState = GAME
          while (Keyboard.next) {}
          newGame
    }
    else if ((Mouse.isButtonDown(0) && Mouse.getX > 257 && Mouse.getX < 514 && 
        (height - Mouse.getY) > 600 && (height - Mouse.getY) < 665)) {
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
    if ((Mouse.isButtonDown(0) && Mouse.getX > 905 && Mouse.getX < 1162 && 
        (height - Mouse.getY) > 316 && (height - Mouse.getY) < 381) || 
        Keyboard.isKeyDown(Keyboard.KEY_N)) {
      gameState = CHARACTER_CREATION
      while (Keyboard.next) {}
      player = new Player(player.name, 0, 0)
    }
    else if ((Mouse.isButtonDown(0) && Mouse.getX > 905 && Mouse.getX < 1162 && 
        (height - Mouse.getY) > 416 && (height - Mouse.getY) < 481) || 
        Keyboard.isKeyDown(Keyboard.KEY_Q)) {
      Display.destroy()
      System.exit(0)
    }
  }
  
  /** Follow user input in game */
  def gameKeys {
    if (Mouse.isButtonDown(0) && getPlayer.experience >= getPlayer.smallestLevel && Mouse.getX > 1080 &&
        Mouse.getX < 1255 && (height - Mouse.getY) > 242 && (height - Mouse.getY) < 307) {
      gameState = LEVEL
      while (Keyboard.next) {}
    }
    else if (Mouse.isButtonDown(0) && Mouse.getX > 1080 && Mouse.getX < 1255 && 
        (height - Mouse.getY) > 640 && (height - Mouse.getY) < 705) {
      gameState = MAIN_MENU
      while (Keyboard.next) {}
    }
    else {
      while (Keyboard.next) {
        Keyboard.getEventKey match {
          case k if (k == Keyboard.KEY_ESCAPE) => {
            gameState = MAIN_MENU
            while (Keyboard.next) {}
          }
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
  }
  
  /** Play one turn */
  def playTurn() {
    for (monster <- monsterList)
      monster.turn
    turn += 1
    if (player.health <= 0) Output.addLog("You died! Score: " + getPlayer.gold.toInt)
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