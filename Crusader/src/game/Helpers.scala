package game

import Main._
import org.newdawn.slick.util.ResourceLoader
import org.newdawn.slick.opengl.{Texture, TextureLoader}
import java.io.{IOException, InputStream}
import scala.collection.mutable.Buffer
import Math.abs

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