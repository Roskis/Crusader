package game

import scala.collection.mutable.Buffer
import scala.util.Random

import org.lwjgl.input.{Keyboard, Mouse}
import org.lwjgl.opengl.{Display, DisplayMode}

import org.newdawn.slick.Color

import Math.abs

import Helpers._
import Direction._
import GameState._
import Prayers._
import Output._
import MonsterType.levelChance

import java.io._
import scala.io.Source

/** Main object is responsible for the allocation of tasks to other parts of the program and 
 *  running the mainloop.
 */
object Main {
  private val rnd = new Random
  private var gameState: GameState.Value = MAIN_MENU
  private var monsterList = Buffer[Monster]()
  private var passiveObjectList = Buffer[PassiveObject]()
  private var equipmentList = Buffer[Equipment]()
  private var useableList = Buffer[Useable]()
  private var gameLog = Buffer[Buffer[(String, Color)]]()
  private var lastWheel: Int = 0
  private var lastMonster: Monster = null
  private var prevMouseState: Boolean = false
  private var monsterChances = Map[MonsterType.Monster, Int]()
  private var itemChances = Map[ItemType.Item, Int]()
  private var shopChances = Map[ItemType.Item, Int]()
  private var shopVisited = false
  private var player: Player = null
  private var grid: Grid = null
  private var episode: Int = 1
  private var level: Int = 1
  private var turn: Int = 0
  private var goNextLevel = false
  
  private val bbp: Int = 32
  private val frameRate: Int = 60
  private val height: Int = 720
  private val width: Int = 1280
  private val version: String = "0.1"
  private var displayModes = Array[DisplayMode]()
  for (mode <- Display.getAvailableDisplayModes) 
    if (mode.isFullscreenCapable && mode.getWidth == width && mode.getHeight == height && mode.getFrequency == frameRate)
      displayModes = displayModes :+ mode
  displayModes = displayModes.sortWith(_.getBitsPerPixel > _.getBitsPerPixel)
  displayModes = displayModes :+ new DisplayMode(width, height)
  
  private var fullscreen = false
  private val options = Source.fromFile("options.txt")
  try {
    for (row <- options.getLines.toVector) {
      if (row.isEmpty || row.head == "#".head || row.head == "/".head) {}
      else if (row.contains("=")) row match {
        case r if (r.split("=")(0).trim.toLowerCase == "fullscreen") => {
          val full = r.split("=")(1).trim.toLowerCase
          if (full == "true" || full == "yes" || full == "1") fullscreen = true && getDisplayMode.isFullscreenCapable
          else fullscreen = false
        }
        case _ => {}
      }
    }
  } finally options.close
  
  var buttonContinue: Button = null
  var buttonNewGameMenu: Button = null
  var buttonOptions: Button = null
  var buttonCredits: Button = null
  var buttonExit: Button = null
  var buttonNewGameChar: Button = null
  var buttonBackChar: Button = null
  var buttonRandom: Button = null
  var buttonXP: Button = null
  var buttonQuit: Button = null
  var buttonBackLVL: Button = null
  var buttonCharity: Button = null
  var buttonDiligence: Button = null
  var buttonHumility: Button = null
  var buttonKindness: Button = null
  var buttonPatience: Button = null
  var buttonTemperance: Button = null
  var buttonZeal: Button = null
  var buttonBackOpt: Button = null
  var buttonBackCre: Button = null
  var buttonPray:Button = null
  
  /** The program's main method.
    *
    * The program will start here. Mainloop includes the creation for window, processing the user 
    * input and asking Output to draw window.
    */
  def main(args:Array[String]) {      
    startDisplay()
    player = new Player("", 0, 0)
    /** Gameloop */
    while (!Display.isCloseRequested) {
      gameState match {
        case g if (g == GAME) => {
          gameKeys
          drawGame
        }
        case g if (g == MAIN_MENU) => {
          menuKeys
          drawMainMenu
        }
        case g if (g == OPTIONS) => {
          optionKeys
          drawOptions
        }
        case g if (g == CREDITS) => {
          creditsKeys
          drawCredits
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
    if (new File("save.dat").exists) new File("save.dat").delete
    gameLog.clear
    addLog("Your holy mission is to purify the world from evil.")
    clearLists
    level = 0
    episode = 1
    turn = 0
    nextMap()
  }
  
  /** Advance one level */
  def nextMap() {
    goNextLevel = false
    drawQuadTex(loading, (getWidth-loading.getImageWidth)/2, (getHeight-loading.getImageHeight)/2, 
        loading.getImageWidth, loading.getImageHeight)
    Display.update
    Display.sync(frameRate)
    updateLastMonster(null)
    shopVisited = false
    clearLists
    level += 1
    updateChances
    grid = new Grid()
    grid.init
    grid.getTile(player.getX, player.getY).addObject(player)
    saveGame
  }
  
  /** Follow user input when choosing new level */
  def levelKeys {
    if (Mouse.isButtonDown(0) && buttonBackLVL.isMouseWithin) {
      gameState = GAME
      while (Keyboard.next) {}
    }
    else if (player.health > 0) {
      if (Mouse.isButtonDown(0) && player.experience >= xpNeededForLevel(player.charity.toInt) && 
          player.charity < 10 && !prevMouseState && buttonCharity.isMouseWithin) {
        player.experience -= xpNeededForLevel(player.charity.toInt)
        player.charity += 1
      } else if (Mouse.isButtonDown(0) && player.experience >= xpNeededForLevel(player.diligence.toInt) && 
          player.diligence < 10 && !prevMouseState && buttonDiligence.isMouseWithin) {
        player.experience -= xpNeededForLevel(player.diligence.toInt)
        player.diligence += 1
      } else if (Mouse.isButtonDown(0) && player.experience >= xpNeededForLevel(player.humility.toInt) && 
          player.humility < 10 && !prevMouseState && buttonHumility.isMouseWithin) {
        player.experience -= xpNeededForLevel(player.humility.toInt)
        player.humility += 1
      } else if (Mouse.isButtonDown(0) && player.experience >= xpNeededForLevel(player.kindness.toInt) && 
          player.kindness < 10 && !prevMouseState && buttonKindness.isMouseWithin) {
        player.experience -= xpNeededForLevel(player.kindness.toInt)
        player.kindness += 1
      } else if (Mouse.isButtonDown(0) && player.experience >= xpNeededForLevel(player.patience.toInt) && 
          player.patience < 10 && !prevMouseState && buttonPatience.isMouseWithin) {
        player.experience -= xpNeededForLevel(player.patience.toInt)
        player.patience += 1
      } else if (Mouse.isButtonDown(0) && player.experience >= xpNeededForLevel(player.temperance.toInt) && 
          player.temperance < 10 && !prevMouseState && buttonTemperance.isMouseWithin) {
        player.experience -= xpNeededForLevel(player.temperance.toInt)
        player.temperance += 1
      } else if (Mouse.isButtonDown(0) && player.experience >= xpNeededForLevel(player.zeal.toInt) && 
          player.zeal < 10 && !prevMouseState && buttonZeal.isMouseWithin) {
        player.experience -= xpNeededForLevel(player.zeal.toInt)
        player.zeal += 1
      } else while (Keyboard.next) {
        Keyboard.getEventKey match {
          case k if (k == Keyboard.KEY_Q && Keyboard.getEventKeyState) => {
            gameState = GAME
            while (Keyboard.next) {} 
          }
          case k if (k == Keyboard.KEY_1 && Keyboard.getEventKeyState && 
              player.experience >= xpNeededForLevel(player.charity.toInt) && player.charity < 10) => {
            player.experience -= xpNeededForLevel(player.charity.toInt)
            player.charity += 1
          }
          case k if (k == Keyboard.KEY_2 && Keyboard.getEventKeyState && 
              player.experience >= xpNeededForLevel(player.diligence.toInt) && player.diligence < 10) => {
            player.experience -= xpNeededForLevel(player.diligence.toInt)
            player.diligence += 1
          }
          case k if (k == Keyboard.KEY_3 && Keyboard.getEventKeyState && 
              player.experience >= xpNeededForLevel(player.humility.toInt) && player.humility < 10) => {
            player.experience -= xpNeededForLevel(player.humility.toInt)
            player.humility += 1
          }
          case k if (k == Keyboard.KEY_4 && Keyboard.getEventKeyState && 
              player.experience >= xpNeededForLevel(player.kindness.toInt) && player.kindness < 10) => {
            player.experience -= xpNeededForLevel(player.kindness.toInt)
            player.kindness += 1
          }
          case k if (k == Keyboard.KEY_5 && Keyboard.getEventKeyState && 
              player.experience >= xpNeededForLevel(player.patience.toInt) && player.patience < 10) => {
            player.experience -= xpNeededForLevel(player.patience.toInt)
            player.patience += 1
          }
          case k if (k == Keyboard.KEY_6 && Keyboard.getEventKeyState && 
              player.experience >= xpNeededForLevel(player.temperance.toInt) && player.temperance < 10) => {
            player.experience -= xpNeededForLevel(player.temperance.toInt)
            player.temperance += 1
          }
          case k if (k == Keyboard.KEY_7 && Keyboard.getEventKeyState && 
              player.experience >= xpNeededForLevel(player.zeal.toInt) && player.zeal < 10) => {
            player.experience -= xpNeededForLevel(player.zeal.toInt)
            player.zeal += 1
          }
          case _ => {}
        }
      }
    }
    prevMouseState = Mouse.isButtonDown(0)
  }
  
  /** Follow user input when creating new character */
  def characterKeys {
    if (Mouse.isButtonDown(0) && buttonNewGameChar.isMouseWithin && player.name != "" && 
        Output.font.getWidth(getPlayer.name) < 700) {
          gameState = GAME
          while (Keyboard.next) {}
          newGame
    }
    else if (Mouse.isButtonDown(0) && buttonBackChar.isMouseWithin) {
      gameState = MAIN_MENU
      while (Keyboard.next) {}
    }
    else if (Mouse.isButtonDown(0) && buttonRandom.isMouseWithin) {
      player.name = randomName
    }
    else while (Keyboard.next) {
      Keyboard.getEventKey match {
        case k if (k == Keyboard.KEY_ESCAPE) => gameState = MAIN_MENU
        case k if (k == Keyboard.KEY_RETURN && Keyboard.getEventKeyState && player.name != "" && Output.font.getWidth(getPlayer.name) < 700) => {
          gameState = GAME
          while (Keyboard.next) {}
          newGame
        }
        case k if (k == Keyboard.KEY_BACK && Keyboard.getEventKeyState) => player.name = player.name.dropRight(1)
        case k if (Keyboard.getEventKeyState) => {
          if (k == 57 && Keyboard.getEventKeyState) player.name = player.name + " "
          else if ((k == 0 || k == 12 || k == 13 || k == 15 || k == 26 || k == 27 || k == 28 || k == 29 || k == 39 || 
              k == 40 || k == 41 || k == 42 || k == 43 || k > 50) && Keyboard.getEventKeyState) {}
          else if ((Keyboard.isKeyDown(42) || Keyboard.isKeyDown(54)) && Keyboard.getEventKeyState) player.name += Keyboard.getKeyName(k).toUpperCase
          else player.name += Keyboard.getKeyName(k).toLowerCase
          }
        case _ => {}
      }
    }
  }
  
  /** Follow user input in options */
  def creditsKeys {
    if ((Mouse.isButtonDown(0) && buttonBackCre.isMouseWithin)) {
      gameState = MAIN_MENU
      while (Keyboard.next) {}
    } else while (Keyboard.next) {
      Keyboard.getEventKey match {
        case k if (k == Keyboard.KEY_ESCAPE && Keyboard.getEventKeyState) => {
          gameState = MAIN_MENU
          while (Keyboard.next) {}
        }
        case _ => {}
      }
    }
  }
  
  /** Follow user input in options */
  def optionKeys {
    if ((Mouse.isButtonDown(0) && buttonBackOpt.isMouseWithin)) {
      gameState = MAIN_MENU
      while (Keyboard.next) {}
    } else while (Keyboard.next) {
      Keyboard.getEventKey match {
        case k if (Keyboard.isKeyDown(56) && k == Keyboard.KEY_RETURN && Keyboard.getEventKeyState) => {
          fullscreen = !fullscreen
          
          val options = Source.fromFile("options.txt")
          var lines = Vector[String]()
          try {
            for (row <- options.getLines.toVector) {
              if (row.contains("=") && row.split("=")(0).trim.toLowerCase == "fullscreen") 
                lines = ("fullscreen=" + fullscreen.toString) +: lines
              else lines = row +: lines
            }
          } finally options.close
          val file = new PrintWriter("options.txt")
          try for (row <- lines) file.println(row)
          finally file.close
          
          changeResolution
        }
        case k if (k == Keyboard.KEY_ESCAPE && Keyboard.getEventKeyState) => {
          gameState = MAIN_MENU
          while (Keyboard.next) {}
        }
        case _ => {}
      }
    }
  }
  
  /** Follow user input in menu */
  def menuKeys {
    if ((Mouse.isButtonDown(0) && buttonNewGameMenu.isMouseWithin)) {
      gameState = CHARACTER_CREATION
      while (Keyboard.next) {}
      player = new Player(player.name, 0, 0)
    } else if ((Mouse.isButtonDown(0) && buttonExit.isMouseWithin) || 
        Keyboard.isKeyDown(Keyboard.KEY_Q)) {
      Display.destroy()
      System.exit(0)
    } else if ((Mouse.isButtonDown(0) && buttonContinue.isMouseWithin) || 
        Keyboard.isKeyDown(Keyboard.KEY_Q)) {
      if (new File("save.dat").exists && loadGame) {
        gameState = GAME
        while (Keyboard.next) {}
      }
    } else if ((Mouse.isButtonDown(0) && buttonOptions.isMouseWithin)) {
      gameState = OPTIONS
      while (Keyboard.next) {}
    } else if ((Mouse.isButtonDown(0) && buttonCredits.isMouseWithin)) {
      gameState = CREDITS
      while (Keyboard.next) {}
    } else {
      while (Keyboard.next) {
        Keyboard.getEventKey match {
          case k if (k == Keyboard.KEY_N && Keyboard.getEventKeyState) => {
            gameState = CHARACTER_CREATION
            while (Keyboard.next) {}
            player = new Player(player.name, 0, 0)
          }
          case k if (k == Keyboard.KEY_C && Keyboard.getEventKeyState) => {
            if (new File("save.dat").exists && loadGame) {
              gameState = GAME
              while (Keyboard.next) {}
            }
          }
          case k if (k == Keyboard.KEY_O && Keyboard.getEventKeyState) => {
            gameState = OPTIONS
            while (Keyboard.next) {}
          }
          case _ => {}
        }
      }
    }
  }
  
  /** Follow user input in game */
  def gameKeys {
    if (Mouse.isButtonDown(0) && getPlayer.experience >= getPlayer.smallestLevel && 
        buttonXP.isMouseWithin && !prevMouseState) {
      gameState = LEVEL
      while (Keyboard.next) {}
    }
    else if (Mouse.isButtonDown(0) && buttonQuit.isMouseWithin && !prevMouseState) {
      if (player.health > 0) saveGame
      gameState = MAIN_MENU
      while (Keyboard.next) {}
    }
    else if (Mouse.isButtonDown(0) && buttonPray.isMouseWithin && player.health > 0 && !prevMouseState) {
      player.pray
      playTurn
    }
    else if (Mouse.isButtonDown(0) && Mouse.getX >= 1216 && getHeight - Mouse.getY >= 362 && 
        Mouse.getX <= 1246 && getHeight - Mouse.getY <= 392 && player.slotUseable != null) {
      player.slotUseable.use
    }
    else if (Mouse.isButtonDown(0) && !prevMouseState && Mouse.getX < 1056 && 
        (getHeight - Mouse.getY) < 544 && getPlayer.health > 0) {
      val mx = (Mouse.getX * 1.0 / 32 + getPlayer.getX - 16).toInt
      val my = ((getHeight - Mouse.getY) * 1.0 / 32 + getPlayer.getY - 8).toInt
      var path = getGrid.dijkstra(player.getTile, getGrid.getTile(mx, my)).drop(1)
      var enemies = false
      if (mx == getPlayer.getX && my == getPlayer.getY) playTurn
      if (getGrid.isWithinGrid(mx, my)) {
        while (!(enemies || path.isEmpty)) {
          player.moveOrAttack(path(0))
          playTurn
          path = path.drop(1)
          getGrid.FOV
          for (enemy <- getMonsterList) if (enemy.getTile.visible) enemies = true
        }
      }
    }
    else {
      while (Keyboard.next) {
        Keyboard.getEventKey match {
          case k if (k == Keyboard.KEY_ESCAPE) => {
            if (player.health > 0) saveGame
            gameState = MAIN_MENU
            while (Keyboard.next) {}
          }
          case k if ((k == Keyboard.KEY_RIGHT || k == Keyboard.KEY_NUMPAD6 || k == Keyboard.KEY_L) && 
              Keyboard.getEventKeyState && getPlayer.health > 0) => {
            player.moveOrAttack(E)
            playTurn
          }
          case k if ((k == Keyboard.KEY_LEFT || k == Keyboard.KEY_NUMPAD4 || k == Keyboard.KEY_H) && 
              Keyboard.getEventKeyState && getPlayer.health > 0) => {
            player.moveOrAttack(W)
            playTurn
          }
          case k if ((k == Keyboard.KEY_UP || k == Keyboard.KEY_NUMPAD8 || k == Keyboard.KEY_K) && 
              Keyboard.getEventKeyState && getPlayer.health > 0) => {
            player.moveOrAttack(N)
            playTurn
          }
          case k if ((k == Keyboard.KEY_DOWN || k == Keyboard.KEY_NUMPAD2 || k == Keyboard.KEY_J) && 
              Keyboard.getEventKeyState && getPlayer.health > 0) => {
            player.moveOrAttack(S)
            playTurn
          }
          case k if ((k == Keyboard.KEY_NUMPAD7 || k == Keyboard.KEY_Y) && 
              Keyboard.getEventKeyState && getPlayer.health > 0) => {
            player.moveOrAttack(NW)
            playTurn
          }
          case k if ((k == Keyboard.KEY_NUMPAD9 || k == Keyboard.KEY_U) && 
              Keyboard.getEventKeyState && getPlayer.health > 0) => {
            player.moveOrAttack(NE)
            playTurn
          }
          case k if ((k == Keyboard.KEY_NUMPAD3 || k == Keyboard.KEY_N) && 
              Keyboard.getEventKeyState && getPlayer.health > 0) => {
            player.moveOrAttack(SE)
            playTurn
          }
          case k if ((k == Keyboard.KEY_NUMPAD1 || k == Keyboard.KEY_B) && 
              Keyboard.getEventKeyState && getPlayer.health > 0) => {
            player.moveOrAttack(SW)
            playTurn
            }
          case k if ((k == Keyboard.KEY_R || k == 82) && Keyboard.getEventKeyState && getPlayer.health > 0) => {
            player.pray
            playTurn
          }
          case k if ((k == Keyboard.KEY_W || k == Keyboard.KEY_NUMPAD5 || k == 52) && 
              Keyboard.getEventKeyState && getPlayer.health > 0) => {
            player.waitTurn
            playTurn
          }
          case k if ((k == Keyboard.KEY_E || k == 78) && Keyboard.getEventKeyState && getPlayer.health > 0 && 
              player.slotUseable != null) => {
            player.slotUseable.use
            playTurn
          }
          case k if (k == Keyboard.KEY_Q && Keyboard.getEventKeyState && 
              getPlayer.experience >= getPlayer.smallestLevel && getPlayer.health > 0) => {
            gameState = LEVEL
            while (Keyboard.next) {}
          }
          case k if (k == Keyboard.KEY_N && Keyboard.getEventKeyState && getPlayer.health <= 0) => {
            while (Keyboard.next) {}
            player = new Player(player.name, 0, 0)
            newGame
          }
          case k if (k == 13 && Keyboard.getEventKeyState) => addLog("Keys: Numpad/arrows = Movement, Esc = Save&Quit, Q = Level up, W = Wait/Buy item, E = Use item and R = Pray.")
          case _ => {}
        }
      }
      lastWheel = Mouse.getDWheel
    }
    prevMouseState = Mouse.isButtonDown(0)
  }
  
  /** Play one turn */
  def playTurn() {
    if (goNextLevel) nextMap
    for (effect <- player.effectList) effect.turn
    player.effectList.filter(_.duration <= 0) foreach {player.effectList -= _}
    var timestop = false
    for (effect <- player.effectList) if (effect.isInstanceOf[timestop]) timestop = true
    if (!timestop) for (monster <- monsterList) monster.turn
    turn += 1
    if (player.health <= 0 && player.piety >= 200) {
      player.piety = player.piety/2 - 200
      if (player.piety < 0) player.piety = 0
      player.experience = 0
      player.health = player.maxHealth
      addLog("You died, but you were resurrected!")
    }
    else if (player.health <= 0) {
      if (new File("save.dat").exists) new File("save.dat").delete
      addLog("You died! Score: " + getPlayer.gold.toInt + ".")
      addLog("Esc to quit and N to start new game.")
    }
  }
  
  def updateChances() {
    updateMonsterChances
    updateItemChances
    updateShopChances
  }
  
  def clearLists() {
    monsterList.clear
    passiveObjectList.clear
    equipmentList.clear
    useableList.clear
  }
  
  /** Save current game */
  def saveGame = {
    val file = new ObjectOutputStream(new FileOutputStream("save.dat"))
    try {
      file.writeUTF(version)
      
      file.writeInt(turn)
      file.writeInt(level)
      file.writeInt(episode)
      file.writeBoolean(shopVisited)
      
      file.writeInt(3 + monsterList.size + passiveObjectList.size + equipmentList.size + 
          useableList.size)
      file.writeObject(new GameLog(gameLog))
      file.writeObject(player)
      file.writeObject(grid)
      for (obj <- monsterList) file.writeObject(obj)
      for (obj <- passiveObjectList) file.writeObject(obj)
      for (obj <- equipmentList) file.writeObject(obj)
      for (obj <- useableList) file.writeObject(obj)
      
    } finally {
      file.close
    }
  }
  
  /** Load game */
  def loadGame:Boolean = {
    var boo = true
    var list = Buffer[java.lang.Object]()
    val file = new ObjectInputStream(new FileInputStream("save.dat"))
    var toRead: Int = 0
    try {
      if (file.readUTF == version) {
        turn = file.readInt
        level = file.readInt
        episode = file.readInt
        shopVisited = file.readBoolean
        
        toRead = file.readInt
        for (i <- 0 until toRead) list += file.readObject
      }
      else boo = false
    } finally {
      file.close()
    }
    
    clearLists
    for (obj <- list) obj match {
      case o: Monster => monsterList.append(o)
      case o: PassiveObject => passiveObjectList.append(o)
      case o: Equipment => equipmentList.append(o)
      case o: Useable => useableList.append(o)
      case o: GameLog => gameLog = o.log
      case o: Player => player = o
      case o: Grid => grid = o
      case _ => {}
    }
    updateChances
    lastMonster = null
    
    boo
  }
  
  /** Updaters */
  def updateMonsterChances = monsterChances = MonsterType.levelChance(level)
  def updateItemChances = itemChances = ItemType.levelChance(level)
  def updateShopChances = shopChances = ItemType.shopChance(level)
  def updateLastMonster(monster: Monster) = lastMonster = monster
  def visitShop = shopVisited = true 
  
  /** Scale height and width when screenresolution is different. */
  def widthScale() = {1.0*getDisplayMode.getWidth/getWidth}
  def heightScale() = {1.0*getDisplayMode.getHeight/getHeight}
  
  /** Get more good effects when piety is higher and more bad ones with negative piety */
  def getPrayerChances() = {
    var chances = Map(STAIRS -> 10, SMITE -> 10, GOLDGAIN -> 10, EXPERIENCEGAIN -> 10, 
        CLAIRVOYANCE -> 10, ITEM -> 1, FEAR -> 5, AOEDAMAGE -> 5, BLINDINGLIGHT -> 5, 
        REVEALSECRET -> 10, IMMUNITY -> 5, BUFF -> 20, VISION -> 10, LEVELUP -> 2, TIMESTOP -> 1, 
        TEMPBOOST -> 10)
    if (player.piety < 0) chances += (DEMENTIA -> abs(player.piety/10).toInt, 
          EXPERIENCELOSS -> abs(player.piety/10).toInt, GOLDLOSS -> abs(player.piety/10).toInt, 
          LIGHTNINGBOLT -> abs(player.piety/5).toInt, LEVELDOWN -> abs(player.piety/10).toInt)
    else chances += (PARTIALRESTORATION -> (80 + (player.piety/2.5).toInt), 
        FULLRESTORATION -> (20 + (player.piety/2.5).toInt))
    chances
  } 
  
  /** Some getters */
  def getRnd() = rnd
  def getGameState() = gameState
  def getMonsterList() = monsterList
  def getPassiveObjectList() = passiveObjectList
  def getEquipmentList() = equipmentList
  def getUseableList() = useableList
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
  def getMonsterChances() = monsterChances
  def getItemChances() = itemChances
  def getShopChances() = shopChances
  def getLastMonster() = lastMonster
  def getShopVisited() = shopVisited
  def getDisplayMode() = displayModes(0)
  def getFullscreen() = fullscreen
  def goToNextLevel() = goNextLevel = true
  
}