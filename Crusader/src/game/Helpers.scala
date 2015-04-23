package game

import Main._
import org.lwjgl.input.Mouse
import org.newdawn.slick.util.ResourceLoader
import org.newdawn.slick.opengl.{Texture, TextureLoader}
import java.io.{IOException, InputStream}
import scala.collection.mutable.Buffer
import Math.abs


/** Basic button */
class Button(val x: Int, val y: Int, val width: Int, val height: Int, val tex: Texture, val tex2: Texture, val tex3: Texture) {
  def getX = x*widthScale
  def getY = y*heightScale
  def getWidth = width*widthScale
  def getHeight = height*heightScale
  def getDrawX = x-(tex.getImageWidth-width)/2
  def getDrawY = y-(tex.getImageHeight-height)/2
  def isMouseWithin: Boolean = 
    Mouse.getX > getX && 
    Mouse.getX < getX+getWidth && 
    (Main.getHeight * heightScale - Mouse.getY) > getY && 
    (Main.getHeight * heightScale - Mouse.getY) < (getY+getHeight) && 
    Mouse.isInsideWindow
}

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
  val GAME, MAIN_MENU, CHARACTER_CREATION, LEVEL, OPTIONS, CREDITS = Value

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
  
  /** Method to get adjacent directions */
  def getAdjacentDirections(dir: Direction): Tuple2[Direction, Direction] = {
    dir match {
      case d if (d == N) => (NW, NE)
      case d if (d == NE) => (N, E)
      case d if (d == E) => (NE, SE)
      case d if (d == SE) => (E, S)
      case d if (d == S) => (SE, SW)
      case d if (d == SW) => (S, W)
      case d if (d == W) => (SW, NW)
      case d if (d == NW) => (W, N)
      case _ => (N, N)
    }
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
  private val irat1 = loadTexture("Monsters/rat1")
  private val irat2 = loadTexture("Monsters/rat2")
  private val ibat1 = loadTexture("Monsters/bat1")
  private val ibat2 = loadTexture("Monsters/bat2")
  private val isnake1 = loadTexture("Monsters/snake1")
  private val isnake2 = loadTexture("Monsters/snake2")
  private val ispider1 = loadTexture("Monsters/spider1")
  private val ispider2 = loadTexture("Monsters/spider2")
  private val igoblina1 = loadTexture("Monsters/goblina1")
  private val igoblina2 = loadTexture("Monsters/goblina2")
  private val igoblinb1 = loadTexture("Monsters/goblinb1")
  private val igoblinb2 = loadTexture("Monsters/goblinb2")
  private val ihound1 = loadTexture("Monsters/hound1")
  private val ihound2 = loadTexture("Monsters/hound2")
  private val ilizarda1 = loadTexture("Monsters/lizarda1")
  private val ilizarda2 = loadTexture("Monsters/lizarda2")
  private val ilizardb1 = loadTexture("Monsters/lizardb1")
  private val ilizardb2 = loadTexture("Monsters/lizardb2")
  private val ilizardc1 = loadTexture("Monsters/lizardc1")
  private val ilizardc2 = loadTexture("Monsters/lizardc2")
  private val icrocodile1 = loadTexture("Monsters/crocodile1")
  private val icrocodile2 = loadTexture("Monsters/crocodile2")
  private val isloth1 = loadTexture("tempSloth")
  private val isloth2 = loadTexture("tempSloth")
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
  private val ismallhealpotion = loadTexture("Items/smallhealpotion")
  private val ibattleAxeG = loadTexture("Items/battleAxeG")
  private val ibattleAxeE = loadTexture("Player/battleAxeE")
  private val icloth1G = loadTexture("Items/cloth1G")
  private val icloth1E = loadTexture("Player/cloth1E")
  private val icloth2G = loadTexture("Items/cloth2G")
  private val icloth2E = loadTexture("Player/cloth2E")
  private val iironShieldG = loadTexture("Items/ironShieldG")
  private val iironShieldE = loadTexture("Player/ironShieldE")
  private val ikatanaG = loadTexture("Items/katanaG")
  private val ikatanaE = loadTexture("Player/katanaE")
  private val ilargeShieldG = loadTexture("Items/largeShieldG")
  private val ilargeShieldE = loadTexture("Player/largeShieldE")
  private val ishortSwordG = loadTexture("Items/shortSwordG")
  private val ishortSwordE = loadTexture("Player/shortSwordE")
  private val ismallShieldG = loadTexture("Items/smallShieldG")
  private val ismallShieldE = loadTexture("Player/smallShieldE")
  private val isteelArmorG = loadTexture("Items/steelArmorG")
  private val isteelArmorE = loadTexture("Player/steelArmorE")
  private val iclaymoreG = loadTexture("Items/claymoreG")
  private val iclaymoreE = loadTexture("Player/claymoreE")
  private val imonoDaggerG = loadTexture("Items/monoDaggerG")
  private val imonoDaggerE = loadTexture("Player/monoDaggerE")
  private val idualDaggerG = loadTexture("Items/dualDaggerG")
  private val idualDaggerE = loadTexture("Player/dualDaggerE")
  private val igoldArmorG = loadTexture("Items/goldArmorG")
  private val igoldArmorE = loadTexture("Player/goldArmorE")
  private val imagicSwordG = loadTexture("Items/magicSwordG")
  private val imagicSwordE = loadTexture("Player/magicSwordE")
  private val ivikingArmorG = loadTexture("Items/vikingArmorG")
  private val ivikingArmorE = loadTexture("Player/vikingArmorE")
  private val ifog: Texture = loadTexture("Tiles/fog")
  private val igrass1 = loadTexture("Tiles/grass1")
  private val iwall1 = loadTexture("Tiles/wall1")
  private val iStairs = loadTexture("tempStairs")
  private val iDjinnDoorH = loadTexture("Tiles/djinnDoorH")
  private val iDjinnDoorV = loadTexture("Tiles/djinnDoorV")
  private val iDjinnFloor1 = loadTexture("Tiles/djinnFloor1")
  private val iDjinnFloor2 = loadTexture("Tiles/djinnFloor2")
  private val iDjinnFloor3 = loadTexture("Tiles/djinnFloor3")
  private val iDjinnWall1 = loadTexture("Tiles/djinnWall1")
  private val iDjinnWall2 = loadTexture("Tiles/djinnWall2")
  private val iDjinnWall3 = loadTexture("Tiles/djinnWall3")
  private val imouseSelector = loadTexture("UI/mouseSelector")
  private val ibackground = loadTexture("UI/Background")
  private val iemptyBackground = loadTexture("UI/EmptyBackground")
  private val icontinue = loadTexture("UI/Continue")
  private val icontinue2 = loadTexture("UI/Continue2")
  private val inewgame = loadTexture("UI/Newgame")
  private val inewgame2 = loadTexture("UI/Newgame2")
  private val ioptions = loadTexture("UI/Options")
  private val icredits = loadTexture("UI/Credits")
  private val iexit = loadTexture("UI/Exit")
  private val iback = loadTexture("UI/Back")
  private val irandom = loadTexture("UI/Random")
  private val iunseen = loadTexture("Tiles/unseen")
  private val iXPButton = loadTexture("UI/XPButton")
  private val iquit = loadTexture("UI/Quit")
  private val iHPBar = loadTexture("UI/HPBar")
  private val iXPBar = loadTexture("UI/XPBar")
  private val iXPBar2 = loadTexture("UI/XPBar2")
  private val iXPBar3 = loadTexture("UI/XPBar3")
  private val iminimapAltar = loadTexture("UI/minimapAltar")
  private val iminimapDjinn = loadTexture("UI/minimapDjinn")
  private val iminimapEnemy = loadTexture("UI/minimapEnemy")
  private val iminimapFloor = loadTexture("UI/minimapFloor")
  private val iminimapFog = loadTexture("UI/minimapFog")
  private val iminimapItem = loadTexture("UI/minimapItem")
  private val iminimapPlayer = loadTexture("UI/minimapPlayer")
  private val iminimapStairs = loadTexture("UI/minimapStairs")
  private val iminimapWall = loadTexture("UI/minimapWall")
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
  private val iUIBackground = loadTexture("UI/UIBackground")
  private val iUIBackground2 = loadTexture("UI/UIBackground2")
  private val iLevelBackground = loadTexture("UI/LevelBackground")
  private val ifireballch1 = loadTexture("Effects/fireballch1")
  private val ifireballch2 = loadTexture("Effects/fireballch2")
  private val ifireballch3 = loadTexture("Effects/fireballch3")
  private val ifireballch4 = loadTexture("Effects/fireballch4")
  private val ifireballch5 = loadTexture("Effects/fireballch5")
  private val ialtar1 = loadTexture("Environment/altar1")
  private val ialtar2 = loadTexture("Environment/altar2")
  private val idjinn1 = loadTexture("Environment/djinn1")
  private val idjinn2 = loadTexture("Environment/djinn2")
  private val idjinn3 = loadTexture("Environment/djinn3")
  private val ibigTree1 = loadTexture("Environment/bigTree1")
  private val itree1 = loadTexture("Environment/tree1")
  private val irock1 = loadTexture("Environment/rock1")
  private val irock2 = loadTexture("Environment/rock2")
  private val iback3 = loadTexture("UI/Back3")
  private val icharity3 = loadTexture("UI/Charity3")
  private val icontinue3 = loadTexture("UI/Continue3")
  private val icredits3 = loadTexture("UI/Credits3")
  private val idiligence3 = loadTexture("UI/Diligence3")
  private val iexit3 = loadTexture("UI/Exit3")
  private val ihumility3 = loadTexture("UI/Humility3")
  private val ikindness3 = loadTexture("UI/Kindness3")
  private val inewgame3 = loadTexture("UI/Newgame3")
  private val ioptions3 = loadTexture("UI/Options3")
  private val ipatience3 = loadTexture("UI/Patience3")
  private val iquit3 = loadTexture("UI/Quit3")
  private val irandom3 = loadTexture("UI/Random3")
  private val itemperance3 = loadTexture("UI/Temperance3")
  private val izeal3 = loadTexture("UI/Zeal3")
  private val ipray = loadTexture("UI/Pray")
  private val ipray3 = loadTexture("UI/Pray3")
  
  /** Return textures */
  def playerImage = iplayerImage
  def playerGrave = iplayerGrave
  def missing = imissing
  def rat1 = irat1
  def rat2 = irat2
  def bat1 = ibat1
  def bat2 = ibat2
  def snake1 = isnake1
  def snake2 = isnake2
  def spider1 = ispider1
  def spider2 = ispider2
  def goblina1 = igoblina1
  def goblina2 = igoblina2
  def goblinb1 = igoblinb1
  def goblinb2 = igoblinb2
  def hound1 = ihound1
  def hound2 = ihound2
  def lizarda1 = ilizarda1
  def lizarda2 = ilizarda2
  def lizardb1 = ilizardb1
  def lizardb2 = ilizardb2
  def lizardc1 = ilizardc1
  def lizardc2 = ilizardc2
  def crocodile1 = icrocodile1
  def crocodile2 = icrocodile2
  def sloth1 = isloth1
  def sloth2 = isloth2
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
  def smallhealpotion = ismallhealpotion
  def battleAxeG = ibattleAxeG
  def battleAxeE = ibattleAxeE
  def cloth1G = icloth1G
  def cloth1E = icloth1E
  def cloth2G = icloth2G
  def cloth2E = icloth2E
  def ironShieldG = iironShieldG
  def ironShieldE = iironShieldE
  def katanaG = ikatanaG
  def katanaE = ikatanaE
  def largeShieldG = ilargeShieldG
  def largeShieldE = ilargeShieldE
  def shortSwordG = ishortSwordG
  def shortSwordE = ishortSwordE
  def smallShieldG = ismallShieldG
  def smallShieldE = ismallShieldE
  def steelArmorG = isteelArmorG
  def steelArmorE = isteelArmorE
  def claymoreG = iclaymoreG
  def claymoreE = iclaymoreE
  def monoDaggerG = imonoDaggerG
  def monoDaggerE = imonoDaggerE
  def dualDaggerG = idualDaggerG
  def dualDaggerE = idualDaggerE
  def goldArmorG = igoldArmorG
  def goldArmorE = igoldArmorE
  def magicSwordG = imagicSwordG
  def magicSwordE = imagicSwordE
  def vikingArmorG = ivikingArmorG
  def vikingArmorE = ivikingArmorE
  def fog = ifog
  def grass1 = igrass1
  def wall1 = iwall1
  def Stairs = iStairs
  def DjinnDoorH = iDjinnDoorH
  def DjinnDoorV = iDjinnDoorV
  def DjinnFloor1 = iDjinnFloor1
  def DjinnFloor2 = iDjinnFloor2
  def DjinnFloor3 = iDjinnFloor3
  def DjinnWall1 = iDjinnWall1
  def DjinnWall2 = iDjinnWall2
  def DjinnWall3 = iDjinnWall3
  def background = ibackground
  def emptyBackground = iemptyBackground
  def continue = icontinue
  def continue2 = icontinue2
  def newgame = inewgame
  def newgame2 = inewgame2
  def options = ioptions
  def credits = icredits
  def exit = iexit
  def back = iback
  def random = irandom
  def unseen = iunseen
  def XPButton = iXPButton
  def quit = iquit
  def HPBar = iHPBar
  def XPBar = iXPBar
  def XPBar2 = iXPBar2
  def XPBar3 = iXPBar3
  def minimapAltar = iminimapAltar
  def minimapDjinn = iminimapDjinn
  def minimapEnemy = iminimapEnemy
  def minimapFloor = iminimapFloor
  def minimapFog = iminimapFog
  def minimapItem = iminimapItem
  def minimapPlayer = iminimapPlayer
  def minimapStairs = iminimapStairs
  def minimapWall = iminimapWall
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
  def mouseSelector = imouseSelector
  def UIBackground = iUIBackground
  def UIBackground2 = iUIBackground2
  def LevelBackground = iLevelBackground
  def fireballch1 = ifireballch1
  def fireballch2 = ifireballch2
  def fireballch3 = ifireballch3
  def fireballch4 = ifireballch4
  def fireballch5 = ifireballch5
  def altar1 = ialtar1
  def altar2 = ialtar2
  def djinn1 = idjinn1
  def djinn2 = idjinn2
  def djinn3 = idjinn3
  def bigTree1 = ibigTree1
  def tree1 = itree1
  def rock1 = irock1
  def rock2 = irock2
  def back3 = iback3
  def charity3 = icharity3
  def continue3 = icontinue3
  def credits3 = icredits3
  def diligence3 = idiligence3
  def exit3 = iexit3
  def humility3 = ihumility3
  def kindness3 = ikindness3
  def newgame3 = inewgame3
  def options3 = ioptions3
  def patience3 = ipatience3
  def quit3 = iquit3
  def random3 = irandom3
  def temperance3 = itemperance3
  def zeal3 = izeal3
  def pray = ipray
  def pray3 = ipray3
  
  buttonContinue = new Button(905, 200, 256, 64, continue, continue2, continue3)
  buttonNewGameMenu = new Button(905, 280, 256, 64, newgame, newgame2, newgame3)
  buttonOptions = new Button(905, 360, 256, 64, options, options, options3)
  buttonCredits = new Button(905, 440, 256, 64, credits, credits, credits3)
  buttonExit = new Button(905, 520, 256, 64, exit, exit, exit3)
  buttonNewGameChar = new Button(799, 600, 256, 64, newgame, newgame2, newgame3)
  buttonBackChar = new Button(257, 600, 256, 64, back, back, back3)
  buttonRandom = new Button(528, 600, 256, 64, random, random, random3)
  buttonXP = new Button(1085, 180, 166, 26, XPButton, XPButton, XPButton)
  buttonPray = new Button(1070, 222, 200, 64, pray, pray, pray3)
  buttonQuit = new Button(1070, 640, 200, 64, quit, quit, quit3)
  buttonBackLVL = new Button(149, 606, 256, 64, back, back, back3)
  buttonCharity = new Button(149, 117, 256, 64, charity, charity2, charity3)
  buttonDiligence = new Button(149, 187, 256, 64, diligence, diligence2, diligence3)
  buttonHumility = new Button(149, 257, 256, 64, humility, humility2, humility3)
  buttonKindness = new Button(149, 327, 256, 64, kindness, kindness2, kindness3)
  buttonPatience = new Button(149, 397, 256, 64, patience, patience2, patience3)
  buttonTemperance = new Button(149, 467, 256, 64, temperance, temperance2, temperance3)
  buttonZeal = new Button(149, 537, 256, 64, zeal, zeal2, zeal3)
  buttonBackOpt = new Button(512, 592, 256, 64, back, back, back3)
  buttonBackCre = new Button(512, 592, 256, 64, back, back, back3)
  
  /** Few djinn names */
  def djinnName: String = {
    val list = List("aku", "erham", "goham", "halam", "juzam", "mahamoti", "mijae", "ruham", 
        "serendib", "sulam", "zanam")
    list(rnd.nextInt(list.size))
  }
  
  /** Few djinn greetings */
  def shopWelcome: String = {
    val list = List(
        "Greetings stranger, may I interest you in my wares?", 
        "Fine day isn't it, now have a look at my wares.", 
        "Buy or sell my price is always fair.", 
        "Welcome to my shop crusader.", 
        "You'll need a lot more than just gods on your side you know.", 
        "Welcome to my humble market, now let's talk about trade...", 
        "Business is business even in the midst of oblivion.", 
        "Oh hi there adventurer, may I aid you on your journey?", 
        "What do you need, mortal?", 
        "What is it you're looking for, mortal?"
        )
    list(rnd.nextInt(list.size))
  }
  
  /** Greet player when entering the shop */
  def shopGreet = {
    if (!getShopVisited) {
      visitShop
      addLog(getGrid.getDjinn.name.toUpperCase.head + getGrid.getDjinn.name.tail + ": \"" + shopWelcome + "\"")
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
  def chooseRandomPrayer(chances: Map[Prayers.Prayer, Int]) = {
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
      case _ => 1000000
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

  private val firstname = List("John", "Oliver", "Alexander", "William", "Jean", "Peter", "Walter", 
      "Eustace", "Raymond", "Adhemar", "Robert", "Ralph", "Geoffrey", "Thomas", "Guglielmo", "Guy", 
      "Alan", "Raoul", "Louis", "Roger", "Henry", "Amadeus", "Frederick", "Otto", "Milo", "Hamelin",  
      "Philip", "Humphrey", "Stephen", "Gerard", "Miles", "Andronikos", "Richard", "André", "Arthur", 
      "Hubert", "Ranulf", "Alberic", "Conon", "Floris", "Rudolf", "Depolt", "Sibylla", "Eraclius", 
      "Balian", "Reginald", "Gerard", "Conrad", "Wolfger", "Boniface", "Enrico", "Leopold", "Hugh", 
      "Pelagio", "Pedro", "Guérin", "Andrew", "Alamanno", "Balian", "Simon", "Alfonso", "Charles", 
      "Guillaume", "Renaud", "Edward", "Leo", "Giles", "Maud", "Hervey", "Constance", "Pernell", 
      "Patrick", "Edmund", "Walkelin", "Waleran")

  private val lastname = List("Hawkwood", "Hugh", "Baldwin", "Nevsky", "Marshal", "Godfrey", 
      "Boucicaut", "Bohemond", "Tancred", "Herman", "Eustace", "Gervaise", "Amaury", "Bertrand", 
      "Fulcher", "Fulk", "Grenier", "Warner", "William-Jordan", "Gaston", "Centule", "Guinard", 
      "Aicard", "Ramon", "Bartholomew", "Raymond", "Raimbaut", "Roman", "William", "Curthose", 
      "Arnulf", "Rotrou", "Jordan", "Enguerrand", "Embriaco", "Montlhéry", "Gouffier", "Leopold", 
      "Eudes", "Anselm", "Welf", "Ekkehard", "Joscelin", "Dagobert", "Arpin", "Clement", "Theobald", 
      "Ghibbelin", "Hugues", "Humphrey", "Bertrand", "Bures", "Fulk", "Barisan", "Thierry", 
      "Alphonse", "Enguerrand", "Eleanor", "Ottokar", "Amalric", "Melisende", "Manasses", "Galeran", 
      "Grenier", "Barisan", "Balian", "Héribrand", "Gilbert", "Kontostephanos", "Walchelin", 
      "Dandolo", "Galvani", "Kaykaus", "Briwere", "Bertrand", "Girard", "Berenguer", "Renaut", 
      "Amalric")

  private val longerLastname = List("de Clisson", "de le Bourg", "de Saint-Gilles", "de Monteil", 
      "de Guader", "de Payens", "de Warenne", "de Beaumont", "de Provence", "de la Roche", 
      "de Blanchefort", " de Chauvigny", "de Ferrers", "de Ferriers", "de Preaux", "de Plessis", 
      "de Glanvill", "de Vesci", "de Béthune", "de Ridefort", "de Sbalé", "de Montaigu", 
      "de Montfort", "de Sonnac", "de Vichiers", "du Guesclin", "des Roches", "von Erla", 
      "von Salza")

  private val places = List("Bouillon", "Hauteville", "Boulogne", "Salerno", "St. Omer", "Bazoches", 
      "Chartres", "Guines", "Grez", "Béarn", "Bigorre", "Lusignan", "Aguilers", "Le Puy", 
      "Montpellier", "Bayeux", "Chocques", "Hierges", "Vermandois", "Lastours", "St. Gilles", 
      "Nevers", "Aquitaine", "Aura", "Courtenay", "Pisa", "Bourges", "Hestrut", "Arles", "Le Puiset", 
      "Jaffa", "Champagne", "Toron", "Toulouse", "Anjou", "Ibelin", "France", "Dreux", "Courtenay", 
      "Vermandois", "Alsace", "Trencavel", "Châtillon", "Aquitaine", "Lusignan", "Bar", "Savoy", 
      "Montferrat", "Auvergne", "Deuil", "Germany", "Freising", "Styria", "Austria", "Jerusalem", 
      "Milly", "Hierges", "Craon", "Ibelin", "Sancerre", "Montferrat", "Bures", "Plancy", "Milly", 
      "Assailly", "England", "Chauvigny", "Exeter", "Burgundy", "Beaumont", "Bazoches", "Plessis", 
      "Preaux", "Hoveden", "France", "Dreux", "Alsace", "Sancerre", "Courtenay", "Kalden", 
      "Zähringen", "Guelders", "Bohemia", "Sicily", "Sidon", "Châtillon", "Wittelsbach", 
      "Montferrat", "Constantinople", "Antioch", "Cyprus", "Hungary", "Rodez", "Soissons", "Arsuf", 
      "Sidon", "Montbéliard", "Anjou", "Artois")

  private val title = List("Prince of Taranto", "Count of Cerdagne and Berga", "Duke of Normandy", 
      "Lord of Coucy", "Archbishop of Milan", "Earl of Leicester", "Duke of Lower Lorraine", 
      "founder of the County of Edessa", "Prince of Galilee", "Count of Saint-Pol", 
      "Count of Hainaut", "Count of Toulouse", "founder of the County of Tripoli", 
      "Bishop of Le Puy", "Count of Cerdagne", "Count of Berga", "Count of Roussillon", 
      "Archbishop of Arles", "Count of Barcelona", "Count of Orange", "Bishop of Orange", 
      "Patriarch of Jerusalem", "Count of Perche", "Count of Flanders", "Count of Vendôme", 
      "Count of Blois", "Count of Burgundy", "Duke of Burgundy", "Duke of Bavaria", "Lord of Coucy", 
      "Duke of Swabia", "Margrave of Baden", "Earl of Warwick", "Count of Jaffa", 
      "Bishop of Beauvais", "Lord of Béthune", "Holy Roman Emperor", "Count of Holland", 
      "Duke of Austria", "Margrave of Lusatia", "Duke of Brabant", "Count of Holland", 
      "King of Navarre", "Duke of Brittany", "Count of Bar", "Lord of Jaffa", "Count of Brienne", 
      "Earl of Cornwall", "Earl of Leicester", "Count of Poitou", "King of Armenia")

  private val romanNumber = List("II", "III", "IV", "V", "VI", "VII", "VIII", "IX", "X", "XI", "XII", 
      "XIII", "XIV", "XV", "XVI", "II", "III", "IV", "II", "III", "IV", "II", "III", "II", "III", 
      "II", "III", "II", "III", "II", "II", "II")
  
  /** Name generator */
  def randomName: String = {
    rnd.nextInt(100) match {
      case r if (r < 20) => {firstname(rnd.nextInt(firstname.size)) + " " + 
        (lastname ::: longerLastname)(rnd.nextInt((lastname ::: longerLastname).size))}
      case r if (r < 60) => {
        if (rnd.nextInt(5) == 0) 
          (firstname ::: lastname)(rnd.nextInt((firstname ::: lastname).size)) + " " + 
        romanNumber(rnd.nextInt(romanNumber.size)) + " of " + places(rnd.nextInt(places.size))
        else lastname(rnd.nextInt(lastname.size)) + " of " + places(rnd.nextInt(places.size))
        }
      case _ => {
        if (rnd.nextInt(2) == 0) 
          (firstname ::: lastname)(rnd.nextInt((firstname ::: lastname).size)) + " " + 
        romanNumber(rnd.nextInt(romanNumber.size)) + " " + title(rnd.nextInt(title.size))
        else lastname(rnd.nextInt(lastname.size)) + " " + title(rnd.nextInt(title.size))
        }
    }
  }
  
}