package game

import Main._
import org.newdawn.slick.util.ResourceLoader
import org.newdawn.slick.opengl.{Texture, TextureLoader}
import java.io.{IOException, InputStream}
import scala.collection.mutable.Buffer
import Math.abs

/** Simple coordinate system */
class Coordinate(var x: Int, var y: Int) extends Serializable {
  /** Getters and setters for coordinates */
  def getX = x
  def getY = y
  def setX(newX: Int) = x = newX
  def setY(newY: Int) = y = newY
}

/** There are three states of game */
object GameState extends Enumeration {

  type GameState = Value
  val GAME, MAIN_MENU, CHARACTER_CREATION, LEVEL = Value

}

/** There are four main directions and four intermediate directions */
object Direction extends Enumeration {

  private val rnd = getRnd
  
  type Direction = Value
  val N, NE, E, SE, S, SW, W, NW = Value

  /** Method to get new coordinates when moving to the wanted direction.
   *
   * @param dir is the given direction
   * @param x is the current x coordinate
   * @param y is the current y coordinate
   * @return Tuple2 containing new x and y coordinates
   */
  def getCoordinates(dir: Direction, x: Int, y: Int): Coordinate = {
    dir match {
      case d if (d == N) => new Coordinate(x, y-1)
      case d if (d == E) => new Coordinate(x+1, y)
      case d if (d == S) => new Coordinate(x, y+1)
      case d if (d == W) => new Coordinate(x-1, y)
      case d if (d == NE) => new Coordinate(x+1, y-1)
      case d if (d == NW) => new Coordinate(x-1, y-1)
      case d if (d == SE) => new Coordinate(x+1, y+1)
      case d if (d == SW) => new Coordinate(x-1, y+1)
    }
  }
  
  def getCoordinates(dir: Direction, startCoord: Coordinate): Coordinate = {
    getCoordinates(dir: Direction, startCoord.getX, startCoord.getY)
  }
  def getCoordinates(dir: Direction, tile: Tile): Coordinate = {
    getCoordinates(dir: Direction, tile.getX, tile.getY)
  }
  def getCoordinates(dir: Direction, obj: Object): Coordinate = {
    getCoordinates(dir: Direction, obj.getX, obj.getY)
  }
  
  /** Method to get the direction when looking from start coordinate to end coordinate */
  def getDirection(startCoord: Coordinate, endCoord: Coordinate): Direction = {
    var dx = endCoord.getX - startCoord.getX
    var dy = endCoord.getY - startCoord.getY
    var dir: Direction = null
    if (abs(dx) >= abs(dy)) {
      if (dx >= 0) {
        dir = E
      }
      else {
        dir = W
      }
    }
    else {
      if (dy >= 0) {
        dir = S
      }
      else {
        dir = N
      }
    }
    dir
  }
  
  /** Return random direction. Insert 4 if you want to move only with main directions and 8 if you 
   *  want to allow intermediate directions.
   *
   * @param num number of directions allowed
   * @return Direction random direction
   */
  def randomDirection(num: Int): Direction = {
    rnd.nextInt(num) match {
      case r if (r == 0) => N
      case r if (r == 1) => E
      case r if (r == 2) => S
      case r if (r == 3) => W
      case r if (r == 4) => NW
      case r if (r == 5) => NE
      case r if (r == 6) => SW
      case r if (r == 7) => SE
    }
  }
  
}

/** Methods that don't anywhere but are needed everywhere */
object Helpers {

  private val rnd =  getRnd
  
  /** Load textures */
  private val imissing = loadTexture("UI/missing")
  private val iplayerImage =  loadTexture("Player/humanBase")
  private val iplayerGrave =  loadTexture("Environment/grave")
  private val irat = loadTexture("Monsters/rat1")
  private val ibat = loadTexture("Monsters/bat1")
  private val isnake = loadTexture("Monsters/snake1")
  private val ispider = loadTexture("Monsters/spider1")
  private val igoblina = loadTexture("Monsters/goblina1")
  private val igoblinb = loadTexture("Monsters/goblinb1")
  private val ihound = loadTexture("Monsters/hound1")
  private val ilizarda = loadTexture("Monsters/lizarda1")
  private val ilizardb = loadTexture("Monsters/lizardb1")
  private val ilizardc = loadTexture("Monsters/lizardc1")
  private val icrocodile = loadTexture("Monsters/crocodile1")
  private val iknifeG = loadTexture("Items/knifeG")
  private val iknifeE = loadTexture("Player/knifeE")
  private val irobesG = loadTexture("Items/robesG")
  private val irobesE = loadTexture("Player/robesE")
  private val iironArmorG = loadTexture("Items/ironArmorG")
  private val iironArmorE = loadTexture("Player/ironArmorE")
  private val isteelSwordG = loadTexture("Items/steelSwordG")
  private val isteelSwordE = loadTexture("Player/steelSwordE")
  private val iwoodenShieldG = loadTexture("Items/woodenShieldG")
  private val iwoodenShieldE = loadTexture("Player/woodenShieldE")
  private val iratG = loadTexture("Items/ratG")
  private val iratE = loadTexture("Player/ratE")
  private val ifog: Texture = loadTexture("Tiles/fog")
  private val igrass1 = loadTexture("Tiles/grass1")
  private val iwall1 = loadTexture("Tiles/wall1")
  private val itempStairs = loadTexture("tempStairs")
  private val itempDjinnDoorH = loadTexture("Tiles/djinnDoorH")
  private val itempDjinnDoorV = loadTexture("Tiles/djinnDoorV")
  private val itempDjinnFloor = {
    val ran = rnd.nextInt(3)
    if (ran == 0) loadTexture("Tiles/djinnFloor1")
    else if (ran == 1 ) loadTexture("Tiles/djinnFloor2")
    else loadTexture("Tiles/djinnFloor3")
  }
  private val itempDjinnWall = {
    val ran = rnd.nextInt(3)
    if (ran == 0) loadTexture("Tiles/djinnWall1")
    else if (ran == 1 ) loadTexture("Tiles/djinnWall2")
    else loadTexture("Tiles/djinnWall3")
  }
  private val ibackground = loadTexture("tempBackground")
  private val icharacterBackground = loadTexture("tempCharacterBackground")
  private val icontinue = loadTexture("UI/Continue")
  private val icontinue2 = loadTexture("UI/Continue2")
  private val inewgame = loadTexture("UI/Newgame")
  private val inewgame2 = loadTexture("UI/Newgame2")
  private val iexit = loadTexture("UI/Exit")
  private val iback = loadTexture("UI/Back")
  private val iunseen = loadTexture("Tiles/unseen")
  private val iheart = loadTexture("UI/Heart")
  private val iXP = loadTexture("UI/XP")
  private val iXPButton = loadTexture("UI/XPButton")
  private val iXPButton2 = loadTexture("UI/XPButton2")
  private val iquit = loadTexture("UI/Quit")
  private val iHPBar = loadTexture("UI/HPBar")
  private val iXPBar = loadTexture("UI/XPBar")
  private val iblackBorder = loadTexture("UI/BlackBorder")
  private val izeal = loadTexture("UI/Zeal")
  private val izeal2 = loadTexture("UI/Zeal2")
  private val ihumility = loadTexture("UI/Humility")
  private val ihumility2 = loadTexture("UI/Humility2")
  private val itemperance = loadTexture("UI/Temperance")
  private val itemperance2 = loadTexture("UI/Temperance2")
  private val ikindness = loadTexture("UI/Kindness")
  private val ikindness2 = loadTexture("UI/Kindness2")
  private val ipatience = loadTexture("UI/Patience")
  private val ipatience2 = loadTexture("UI/Patience2")
  private val icharity = loadTexture("UI/Charity")
  private val icharity2 = loadTexture("UI/Charity2")
  private val idiligence = loadTexture("UI/Diligence")
  private val idiligence2 = loadTexture("UI/Diligence2")
  private val ilevel = loadTexture("UI/Level")
  private val ilevel2 = loadTexture("UI/Level2")
  private val itempUIBackground = loadTexture("tempUIBackground")
  private val itempUIBackground2 = loadTexture("tempUIBackground2")
  private val itempLevelBackground = loadTexture("tempLevelBackground")
  
  /** Return textures */
  def playerImage = iplayerImage
  def playerGrave = iplayerGrave
  def missing = imissing
  def rat = irat
  def bat = ibat
  def snake = isnake
  def spider = ispider
  def goblina = igoblina
  def goblinb = igoblinb
  def hound = ihound
  def lizarda = ilizarda
  def lizardb = ilizardb
  def lizardc = ilizardc
  def crocodile = icrocodile
  def knifeG = iknifeG
  def knifeE = iknifeE
  def robesG = irobesG
  def robesE = irobesE
  def ironArmorG = iironArmorG
  def ironArmorE = iironArmorE
  def steelSwordG = isteelSwordG
  def steelSwordE = isteelSwordE
  def woodenShieldG = iwoodenShieldG
  def woodenShieldE = iwoodenShieldE
  def ratG = iratG
  def ratE = iratE
  def fog = ifog
  def grass1 = igrass1
  def wall1 = iwall1
  def tempStairs = itempStairs
  def tempDjinnDoorH = itempDjinnDoorH
  def tempDjinnDoorV = itempDjinnDoorV
  def tempDjinnFloor = itempDjinnFloor
  def tempDjinnWall = itempDjinnWall
  def background = ibackground
  def characterBackground = icharacterBackground
  def continue = icontinue
  def continue2 = icontinue2
  def newgame = inewgame
  def newgame2 = inewgame2
  def exit = iexit
  def back = iback
  def unseen = iunseen
  def heart = iheart
  def XP = iXP
  def XPButton = iXPButton
  def XPButton2 = iXPButton2
  def quit = iquit
  def HPBar = iHPBar
  def XPBar = iXPBar
  def blackBorder = iblackBorder
  def zeal = izeal
  def zeal2 = izeal2
  def humility = ihumility
  def humility2 = ihumility2
  def temperance = itemperance
  def temperance2 = itemperance2
  def kindness = ikindness
  def kindness2 = ikindness2
  def patience = ipatience
  def patience2 = ipatience2
  def charity = icharity
  def charity2 = icharity2
  def diligence = idiligence
  def diligence2 = idiligence2
  def level = ilevel
  def level2 = ilevel2
  def tempUIBackground = itempUIBackground
  def tempUIBackground2 = itempUIBackground2
  def tempLevelBackground = itempLevelBackground
  
  /** Few djinn names */
  def djinnName: String = {
    val list = List("Aku", "Erham", "Goham", "Halam", "Juzam", "Mahamoti", "Mijae", "Ruham", 
        "Serendib", "Sulam", "Zanam")
    list(rnd.nextInt(list.size))
  }
  
  /** Few djinn greetings */
  def shopWelcome: String = {
    val list = List("Greetings stranger, may I interest you in my wares?", 
        "Fine day isn't it, now have a look at my wares.", 
        "Buy or sell my price is always fair.", 
        "Welcome to my shop crusader.", 
        "You'll need a lot more than just gods on your side you know.", 
        "Welcome to my humble market, now let's talk about trade...", 
        "Business is business even in the midst of oblivion.", 
        "Oh hi there adventurer, may I aid you on your journey?", 
        "What do you need, mortal?", 
        "What is it you're looking for, mortal?")
    list(rnd.nextInt(list.size))
  }
  
  /** Greet player when entering the shop */
  def shopGreet = {
    if (!getShopVisited) {
      visitShop
      addLog(getGrid.getDjinn.name + ": \"" + shopWelcome + "\"")
    }
  }
  
  /** Select random monster from map containing choices and their chances */
  def chooseRandomItem(chances: Map[ItemType.Item, Int]) = {
    var random = 0
    for (choice <- chances) random += choice._2
    random = rnd.nextInt(random) + 1
    
    var sum = 0
    var choice = chances.head._1
    
    for (i <- chances ; if sum < random) {
      choice = i._1
      sum += i._2
    }
    choice
  }
  
  /** Select random monster from map containing choices and their chances */
  def chooseRandomMonster(chances: Map[MonsterType.Monster, Int]) = {
    var random = 0
    for (choice <- chances) random += choice._2
    random = rnd.nextInt(random) + 1
    
    var sum = 0
    var choice = chances.head._1
    
    for (i <- chances ; if sum < random) {
      choice = i._1
      sum += i._2
    }
    choice
  }
  
  /** Select random prayer from map containing choices and their chances */
  def chooseRandomPrayer(chances: Map[Effect.Prayer, Int]) = {
    var random = 0
    for (choice <- chances) random += choice._2
    random = rnd.nextInt(random) + 1
    
    var sum = 0
    var choice = chances.head._1
    
    for (i <- chances ; if sum < random) {
      choice = i._1
      sum += i._2
    }
    choice
  }
  
  /** Simple way to roll multiple dice */
  def roll(amount: Int, number: Int): Int = {
    var num: Int = 0
    for (dice <- (0 until amount)) num += roll(number)
    num
  }
  
  /** Simple way to roll one dice */
  def roll(number: Int): Int = getRnd.nextInt(number) + 1
  
  /** Return needed amount of experience to level up */
  def xpNeededForLevel(level: Int): Int = {
    level match {
      case l if (l == 0) => 10
      case l if (l == 1) => 25
      case l if (l == 2) => 85
      case l if (l == 3) => 225
      case l if (l == 4) => 750
      case l if (l == 5) => 1800
      case l if (l == 6) => 3000
      case l if (l == 7) => 5000
      case l if (l == 8) => 8000
      case l if (l == 9) => 15000
      case l if (l == 10) => 45000
      case _ => 10
    }
  }
  
  /** Method to load textures from given location.
   *  
   *  Since all images are in png format and in the data folder, it is set already.
   *
   * @param path is location of the texture
   * @return a Texture build from image at the given location
   */
  def loadTexture(path: String): Texture = {
    var tex:Texture = null
    val in:InputStream = ResourceLoader.getResourceAsStream("data/" + path + ".png")
    try {
      tex = TextureLoader.getTexture("PNG", in)
    } catch {
      case e:IOException => e.printStackTrace()
      tex = TextureLoader.getTexture("PNG", ResourceLoader.getResourceAsStream("data/UI/missing.png"))
    }
    tex
  }
  
  /** Method to wrap long strings into smaller lines.
   *
   * @param text is the long text block needed to split
   * @param maxLenght is the maximum lenght (in pixels) for one line
   * @return Buffer containing smaller strings
   */
  def wordWrap(text: String, maxLength: Int): Buffer[String] = {
    val lines = Buffer[String]()
    var line = ""
    for (word <- text.split(" ")) {
      if (Output.font.getWidth(line + " " + word) > maxLength) {
        lines += line
        line = word + " "
      }
      else line += word + " "
    }
    lines += line
    lines
  }
  
  /** Add message to gamelogs */
  def addLog(text: String) = {
    for (line <- wordWrap(text, 1030)) {
      getGameLog.append(line)
      Output.mouseScrollBonus += 1
    }
  }
  
}