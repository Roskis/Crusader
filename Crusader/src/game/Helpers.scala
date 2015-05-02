package game

import Main._
import org.lwjgl.input.Mouse
import org.newdawn.slick.Color
import org.newdawn.slick.util.ResourceLoader
import org.newdawn.slick.opengl.{Texture, TextureLoader}
import java.io.{IOException, InputStream}
import scala.collection.mutable.Buffer
import Math.abs

class GameLog(val log: Buffer[Buffer[(String, Color)]]) extends Serializable {}

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
  private val iloading = loadTexture("UI/Loading")
  private lazy val iplayerImage =  loadTexture("Player/humanBase")
  private lazy val iplayerGrave =  loadTexture("Environment/grave")
  private lazy val irat1 = loadTexture("Monsters/rat1")
  private lazy val irat2 = loadTexture("Monsters/rat2")
  private lazy val ibat1 = loadTexture("Monsters/bat1")
  private lazy val ibat2 = loadTexture("Monsters/bat2")
  private lazy val isnakea1 = loadTexture("Monsters/snakegreen1")
  private lazy val isnakea2 = loadTexture("Monsters/snakegreen2")
  private lazy val isnakeb1 = loadTexture("Monsters/snakeorange1")
  private lazy val isnakeb2 = loadTexture("Monsters/snakeorange2")
  private lazy val ispider1 = loadTexture("Monsters/spider1")
  private lazy val ispider2 = loadTexture("Monsters/spider2")
  private lazy val igoblina1 = loadTexture("Monsters/goblina1")
  private lazy val igoblina2 = loadTexture("Monsters/goblina2")
  private lazy val igoblinb1 = loadTexture("Monsters/goblinb1")
  private lazy val igoblinb2 = loadTexture("Monsters/goblinb2")
  private lazy val ihound1 = loadTexture("Monsters/hound1")
  private lazy val ihound2 = loadTexture("Monsters/hound2")
  private lazy val ilizarda1 = loadTexture("Monsters/lizarda1")
  private lazy val ilizarda2 = loadTexture("Monsters/lizarda2")
  private lazy val ilizardb1 = loadTexture("Monsters/lizardb1")
  private lazy val ilizardb2 = loadTexture("Monsters/lizardb2")
  private lazy val ilizardc1 = loadTexture("Monsters/lizardc1")
  private lazy val ilizardc2 = loadTexture("Monsters/lizardc2")
  private lazy val icrocodile1 = loadTexture("Monsters/crocodile1")
  private lazy val icrocodile2 = loadTexture("Monsters/crocodile2")
  private lazy val isloth1 = loadTexture("tempSloth")
  private lazy val isloth2 = loadTexture("tempSloth")
  private lazy val iknifeG = loadTexture("Items/knifeG")
  private lazy val iknifeE = loadTexture("Player/knifeE")
  private lazy val irobesG = loadTexture("Items/robesG")
  private lazy val irobesE = loadTexture("Player/robesE")
  private lazy val iironArmorG = loadTexture("Items/ironArmorG")
  private lazy val iironArmorE = loadTexture("Player/ironArmorE")
  private lazy val isteelSwordG = loadTexture("Items/steelSwordG")
  private lazy val isteelSwordE = loadTexture("Player/steelSwordE")
  private lazy val iwoodenShieldG = loadTexture("Items/woodenShieldG")
  private lazy val iwoodenShieldE = loadTexture("Player/woodenShieldE")
  private lazy val iratG = loadTexture("Items/ratG")
  private lazy val iratE = loadTexture("Player/ratE")
  private lazy val ismallhealpotion = loadTexture("Items/smallhealpotion")
  private lazy val ibattleAxeG = loadTexture("Items/battleAxeG")
  private lazy val ibattleAxeE = loadTexture("Player/battleAxeE")
  private lazy val icloth1G = loadTexture("Items/cloth1G")
  private lazy val icloth1E = loadTexture("Player/cloth1E")
  private lazy val icloth2G = loadTexture("Items/cloth2G")
  private lazy val icloth2E = loadTexture("Player/cloth2E")
  private lazy val iironShieldG = loadTexture("Items/ironShieldG")
  private lazy val iironShieldE = loadTexture("Player/ironShieldE")
  private lazy val ikatanaG = loadTexture("Items/katanaG")
  private lazy val ikatanaE = loadTexture("Player/katanaE")
  private lazy val ilargeShieldG = loadTexture("Items/largeShieldG")
  private lazy val ilargeShieldE = loadTexture("Player/largeShieldE")
  private lazy val ishortSwordG = loadTexture("Items/shortSwordG")
  private lazy val ishortSwordE = loadTexture("Player/shortSwordE")
  private lazy val ismallShieldG = loadTexture("Items/smallShieldG")
  private lazy val ismallShieldE = loadTexture("Player/smallShieldE")
  private lazy val isteelArmorG = loadTexture("Items/steelArmorG")
  private lazy val isteelArmorE = loadTexture("Player/steelArmorE")
  private lazy val iclaymoreG = loadTexture("Items/claymoreG")
  private lazy val iclaymoreE = loadTexture("Player/claymoreE")
  private lazy val imonoDaggerG = loadTexture("Items/monoDaggerG")
  private lazy val imonoDaggerE = loadTexture("Player/monoDaggerE")
  private lazy val idualDaggerG = loadTexture("Items/dualDaggerG")
  private lazy val idualDaggerE = loadTexture("Player/dualDaggerE")
  private lazy val igoldArmorG = loadTexture("Items/goldArmorG")
  private lazy val igoldArmorE = loadTexture("Player/goldArmorE")
  private lazy val imagicSwordG = loadTexture("Items/magicSwordG")
  private lazy val imagicSwordE = loadTexture("Player/magicSwordE")
  private lazy val ivikingArmorG = loadTexture("Items/vikingArmorG")
  private lazy val ivikingArmorE = loadTexture("Player/vikingArmorE")
  private lazy val ifog: Texture = loadTexture("Tiles/fog")
  private lazy val ifloorEp1 = loadTexture("Tiles/floorEp1")
  private lazy val iportalEp1 = loadTexture("Environment/portalEp1")
  private lazy val igrass1 = loadTexture("Environment/grass1")
  private lazy val igrass2 = loadTexture("Environment/grass2")
  private lazy val igrass3 = loadTexture("Environment/grass3")
  private lazy val iflower1 = loadTexture("Environment/flower1")
  private lazy val iflower2 = loadTexture("Environment/flower2")
  private lazy val iflower3 = loadTexture("Environment/flower3")
  private lazy val iflower4 = loadTexture("Environment/flower4")
  private lazy val iflower5 = loadTexture("Environment/flower5")
  private lazy val iflower6 = loadTexture("Environment/flower6")
  private lazy val iflower7 = loadTexture("Environment/flower7")
  private lazy val iflower8 = loadTexture("Environment/flower8")
  private lazy val iflower9 = loadTexture("Environment/flower9")
  private lazy val iflower10 = loadTexture("Environment/flower10")
  private lazy val iflower11 = loadTexture("Environment/flower11")
  private lazy val ilake1 = loadTexture("Environment/lake1")
  private lazy val ilake2 = loadTexture("Environment/lake2")
  private lazy val ilake3 = loadTexture("Environment/lake3")
  private lazy val ilake4 = loadTexture("Environment/lake4")
  private lazy val ibigFlower1 = loadTexture("Environment/bigFlower1")
  private lazy val ibigFlower2 = loadTexture("Environment/bigFlower2")
  private lazy val ibigFlower3 = loadTexture("Environment/bigFlower3")
  private lazy val ibigFlower4 = loadTexture("Environment/bigFlower4")
  private lazy val ibigFlower5 = loadTexture("Environment/bigFlower5")
  private lazy val ibigFlower6 = loadTexture("Environment/bigFlower6")
  private lazy val ibush1 = loadTexture("Environment/bush1")
  private lazy val ibush2 = loadTexture("Environment/bush2")
  private lazy val ibush3 = loadTexture("Environment/bush3")
  private lazy val ibush4 = loadTexture("Environment/bush4")
  private lazy val ibush5 = loadTexture("Environment/bush5")
  private lazy val ibush6 = loadTexture("Environment/bush6")
  private lazy val ibush7 = loadTexture("Environment/bush7")
  private lazy val ismallRock1 = loadTexture("Environment/smallRock1")
  private lazy val ismallRock2 = loadTexture("Environment/smallRock2")
  private lazy val ismallRock3 = loadTexture("Environment/smallRock3")
  private lazy val ismallRock4 = loadTexture("Environment/smallRock4")
  private lazy val ismallRock5 = loadTexture("Environment/smallRock5")
  private lazy val ismallRock6 = loadTexture("Environment/smallRock6")
  private lazy val ismallRock7 = loadTexture("Environment/smallRock7")
  private lazy val ibigBush1 = loadTexture("Environment/bigBush1")
  private lazy val ibigBush2 = loadTexture("Environment/bigBush2")
  private lazy val ibigBush3 = loadTexture("Environment/bigBush3")
  private lazy val ibigBush4 = loadTexture("Environment/bigBush4")
  private lazy val ifern1 = loadTexture("Environment/fern1")
  private lazy val ifern2 = loadTexture("Environment/fern2")
  private lazy val ifern3 = loadTexture("Environment/fern3")
  private lazy val ifern4 = loadTexture("Environment/fern4")
  private lazy val iblood1 = loadTexture("Environment/blood1")
  private lazy val iblood2 = loadTexture("Environment/blood2")
  private lazy val iblood3 = loadTexture("Environment/blood3")
  private lazy val iblood4 = loadTexture("Environment/blood4")
  private lazy val iblood5 = loadTexture("Environment/blood5")
  private lazy val iblood6 = loadTexture("Environment/blood6")
  private lazy val iblood7 = loadTexture("Environment/blood7")
  private lazy val iwall1 = loadTexture("Tiles/wall1")
  private lazy val iStairs = loadTexture("tempStairs")
  private lazy val iDjinnDoorH = loadTexture("Tiles/djinnDoorH")
  private lazy val iDjinnDoorV = loadTexture("Tiles/djinnDoorV")
  private lazy val iDjinnFloor1 = loadTexture("Tiles/djinnFloor1")
  private lazy val iDjinnFloor2 = loadTexture("Tiles/djinnFloor2")
  private lazy val iDjinnFloor3 = loadTexture("Tiles/djinnFloor3")
  private lazy val iDjinnWall1 = loadTexture("Tiles/djinnWall1")
  private lazy val iDjinnWall2 = loadTexture("Tiles/djinnWall2")
  private lazy val iDjinnWall3 = loadTexture("Tiles/djinnWall3")
  private lazy val imouseSelector = loadTexture("UI/mouseSelector")
  private lazy val ibackground = loadTexture("UI/Background")
  private lazy val iemptyBackground = loadTexture("UI/EmptyBackground")
  private lazy val icontinue = loadTexture("UI/Continue")
  private lazy val icontinue2 = loadTexture("UI/Continue2")
  private lazy val inewgame = loadTexture("UI/Newgame")
  private lazy val inewgame2 = loadTexture("UI/Newgame2")
  private lazy val ioptions = loadTexture("UI/Options")
  private lazy val icredits = loadTexture("UI/Credits")
  private lazy val iexit = loadTexture("UI/Exit")
  private lazy val iback = loadTexture("UI/Back")
  private lazy val irandom = loadTexture("UI/Random")
  private lazy val iunseen = loadTexture("Tiles/unseen")
  private lazy val iXPButton = loadTexture("UI/XPButton")
  private lazy val iquit = loadTexture("UI/Quit")
  private lazy val iHPBar = loadTexture("UI/HPBar")
  private lazy val iXPBar = loadTexture("UI/XPBar")
  private lazy val iXPBar2 = loadTexture("UI/XPBar2")
  private lazy val iXPBar3 = loadTexture("UI/XPBar3")
  private lazy val iminimapAltar = loadTexture("UI/minimapAltar")
  private lazy val iminimapDjinn = loadTexture("UI/minimapDjinn")
  private lazy val iminimapEnemy = loadTexture("UI/minimapEnemy")
  private lazy val iminimapFloor = loadTexture("UI/minimapFloor")
  private lazy val iminimapFog = loadTexture("UI/minimapFog")
  private lazy val iminimapItem = loadTexture("UI/minimapItem")
  private lazy val iminimapPlayer = loadTexture("UI/minimapPlayer")
  private lazy val iminimapStairs = loadTexture("UI/minimapStairs")
  private lazy val iminimapWall = loadTexture("UI/minimapWall")
  private lazy val iblackBorder = loadTexture("UI/BlackBorder")
  private lazy val izeal = loadTexture("UI/Zeal")
  private lazy val izeal2 = loadTexture("UI/Zeal2")
  private lazy val ihumility = loadTexture("UI/Humility")
  private lazy val ihumility2 = loadTexture("UI/Humility2")
  private lazy val itemperance = loadTexture("UI/Temperance")
  private lazy val itemperance2 = loadTexture("UI/Temperance2")
  private lazy val ikindness = loadTexture("UI/Kindness")
  private lazy val ikindness2 = loadTexture("UI/Kindness2")
  private lazy val ipatience = loadTexture("UI/Patience")
  private lazy val ipatience2 = loadTexture("UI/Patience2")
  private lazy val icharity = loadTexture("UI/Charity")
  private lazy val icharity2 = loadTexture("UI/Charity2")
  private lazy val idiligence = loadTexture("UI/Diligence")
  private lazy val idiligence2 = loadTexture("UI/Diligence2")
  private lazy val ilevel = loadTexture("UI/Level")
  private lazy val ilevel2 = loadTexture("UI/Level2")
  private lazy val iUIBackground = loadTexture("UI/UIBackground")
  private lazy val iUIBackground2 = loadTexture("UI/UIBackground2")
  private lazy val iLevelBackground = loadTexture("UI/LevelBackground")
  private lazy val ifireballch1 = loadTexture("Effects/fireballch1")
  private lazy val ifireballch2 = loadTexture("Effects/fireballch2")
  private lazy val ifireballch3 = loadTexture("Effects/fireballch3")
  private lazy val ifireballch4 = loadTexture("Effects/fireballch4")
  private lazy val ifireballch5 = loadTexture("Effects/fireballch5")
  private lazy val ialtar1 = loadTexture("Environment/altar1")
  private lazy val ialtar2 = loadTexture("Environment/altar2")
  private lazy val idjinn11 = loadTexture("Monsters/djinn11")
  private lazy val idjinn21 = loadTexture("Monsters/djinn21")
  private lazy val idjinn31 = loadTexture("Monsters/djinn31")
  private lazy val idjinn12 = loadTexture("Monsters/djinn12")
  private lazy val idjinn22 = loadTexture("Monsters/djinn22")
  private lazy val idjinn32 = loadTexture("Monsters/djinn32")
  private lazy val ibigTree1 = loadTexture("Environment/bigTree1")
  private lazy val itree1 = loadTexture("Environment/tree1")
  private lazy val irock1 = loadTexture("Environment/rock1")
  private lazy val irock2 = loadTexture("Environment/rock2")
  private lazy val iback3 = loadTexture("UI/Back3")
  private lazy val icharity3 = loadTexture("UI/Charity3")
  private lazy val icontinue3 = loadTexture("UI/Continue3")
  private lazy val icredits3 = loadTexture("UI/Credits3")
  private lazy val idiligence3 = loadTexture("UI/Diligence3")
  private lazy val iexit3 = loadTexture("UI/Exit3")
  private lazy val ihumility3 = loadTexture("UI/Humility3")
  private lazy val ikindness3 = loadTexture("UI/Kindness3")
  private lazy val inewgame3 = loadTexture("UI/Newgame3")
  private lazy val ioptions3 = loadTexture("UI/Options3")
  private lazy val ipatience3 = loadTexture("UI/Patience3")
  private lazy val iquit3 = loadTexture("UI/Quit3")
  private lazy val irandom3 = loadTexture("UI/Random3")
  private lazy val itemperance3 = loadTexture("UI/Temperance3")
  private lazy val izeal3 = loadTexture("UI/Zeal3")
  private lazy val ipray = loadTexture("UI/Pray")
  private lazy val ipray3 = loadTexture("UI/Pray3")
  private lazy val ibuffpotion = loadTexture("Items/buffpotion")
  private lazy val icharitypotion = loadTexture("Items/charitypotion")
  private lazy val idiligencepotion = loadTexture("Items/diligencepotion")
  private lazy val ihumilitypotion = loadTexture("Items/humilitypotion")
  private lazy val iimmunitypotion = loadTexture("Items/immunitypotion")
  private lazy val ikindnesspotion = loadTexture("Items/kindnesspotion")
  private lazy val ipatiencepotion = loadTexture("Items/patiencepotion")
  private lazy val itemperancepotion = loadTexture("Items/temperancepotion")
  private lazy val itimepotion = loadTexture("Items/timepotion")
  private lazy val ivisionpotion = loadTexture("Items/visionpotion")
  private lazy val izealpotion = loadTexture("Items/zealpotion")
  private lazy val igreyBackground = loadTexture("UI/greyBackground")
  
  /** Return textures */
  def playerImage = iplayerImage
  def playerGrave = iplayerGrave
  def missing = imissing
  def rat1 = irat1
  def rat2 = irat2
  def bat1 = ibat1
  def bat2 = ibat2
  def snakea1 = isnakea1
  def snakea2 = isnakea2
  def snakeb1 = isnakeb1
  def snakeb2 = isnakeb2
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
  def floorEp1 = ifloorEp1
  def portalEp1 = iportalEp1
  def grass1 = igrass1
  def grass2 = igrass2
  def grass3 = igrass3
  def flower1 = iflower1
  def flower2 = iflower2
  def flower3 = iflower3
  def flower4 = iflower4
  def flower5 = iflower5
  def flower6 = iflower6
  def flower7 = iflower7
  def flower8 = iflower8
  def flower9 = iflower9
  def flower10 = iflower10
  def flower11 = iflower11
  def lake1 = ilake1
  def lake2 = ilake2
  def lake3 = ilake3
  def lake4 = ilake4
  def bigFlower1 = ibigFlower1
  def bigFlower2 = ibigFlower2
  def bigFlower3 = ibigFlower3
  def bigFlower4 = ibigFlower4
  def bigFlower5 = ibigFlower5
  def bigFlower6 = ibigFlower6
  def bush1 = ibush1
  def bush2 = ibush2
  def bush3 = ibush3
  def bush4 = ibush4
  def bush5 = ibush5
  def bush6 = ibush6
  def bush7 = ibush7
  def smallRock1 = ismallRock1
  def smallRock2 = ismallRock2
  def smallRock3 = ismallRock3
  def smallRock4 = ismallRock4
  def smallRock5 = ismallRock5
  def smallRock6 = ismallRock6
  def smallRock7 = ismallRock7
  def bigBush1 = ibigBush1
  def bigBush2 = ibigBush2
  def bigBush3 = ibigBush3
  def bigBush4 = ibigBush4
  def fern1 = ifern1
  def fern2 = ifern2
  def fern3 = ifern3
  def fern4 = ifern4
  def blood1 = iblood1
  def blood2 = iblood2
  def blood3 = iblood3
  def blood4 = iblood4
  def blood5 = iblood5
  def blood6 = iblood6
  def blood7 = iblood7
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
  def djinn11 = idjinn11
  def djinn21 = idjinn21
  def djinn31 = idjinn31
  def djinn12 = idjinn12
  def djinn22 = idjinn22
  def djinn32 = idjinn32
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
  def buffpotion = ibuffpotion
  def charitypotion = icharitypotion
  def diligencepotion = idiligencepotion
  def humilitypotion = ihumilitypotion
  def immunitypotion = iimmunitypotion
  def kindnesspotion = ikindnesspotion
  def patiencepotion = ipatiencepotion
  def temperancepotion = itemperancepotion
  def timepotion = itimepotion
  def visionpotion = ivisionpotion
  def zealpotion = izealpotion
  def loading = iloading
  def greyBackground = igreyBackground
  
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
    if (amount != 0 && number != 0) {
      var num: Int = 0
      for (dice <- (0 until amount)) num += roll(number)
      num
    }
    else 0
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
  
  /** Add message to gamelogs */
  def addLog(text: String) = {
    val lines = Buffer[String]()
    var line = ""
    for (word <- text.split(" ")) {
      if (Output.font.getWidth(line + " " + word) > 1030) {
        lines += line
        line = word + " "
      }
      else line += word + " "
    }
    lines += line
    for (line <- lines) {
      getGameLog.append((Buffer((line, Color.black))))
      Output.mouseScrollBonus += 1
    }
  }
  
  /** Add message to gamelogs */
  def addLog(textBlock: Buffer[(String, Color)]) = {
    var line = Buffer[(String, Color)]()
    var x = 0
    for (text <- textBlock) {
      if (x + Output.font.getWidth(text._1) < 1030) {
        line.append(text)
        x += Output.font.getWidth(text._1)
      }
      else {
        for (word <- text._1.split(" ")) {
          if (x + Output.font.getWidth(word) < 1030) {
            line.append((word + " ", text._2))
            x += Output.font.getWidth(word + " ")
          }
          else {
            getGameLog.append(line)
            Output.mouseScrollBonus += 1
            line = Buffer((word + " ", text._2))
            x = Output.font.getWidth(word + " ")
          }
        }
      }
    }
    getGameLog.append(line)
    Output.mouseScrollBonus += 1
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
  
  def newColor(color: Color): Color = {
    color match {
      case c if (c == Color.black) => new Color(0,0,0,1.0f)
      case c if (c == Color.transparent) => new Color(0.0f,0.0f,0.0f,0.0f)
      case c if (c == Color.white) => new Color(1.0f,1.0f,1.0f,1.0f)
      case c if (c == Color.yellow) => new Color(1.0f,1.0f,0,1.0f)
      case c if (c == Color.red) => new Color(1.0f,0,0,1.0f)
      case c if (c == Color.blue) => new Color(0,0,1.0f,1.0f)
      case c if (c == Color.green) => new Color(0,1.0f,0,1.0f)
      case c if (c == Color.gray) => new Color(0.5f,0.5f,0.5f,1.0f)
      case c if (c == Color.cyan) => new Color(0,1.0f,1.0f,1.0f)
      case c if (c == Color.darkGray) => new Color(0.3f,0.3f,0.3f,1.0f)
      case c if (c == Color.lightGray) => new Color(0.7f,0.7f,0.7f,1.0f)
      case c if (c == Color.pink) => new Color(255, 175, 175, 255)
      case c if (c == Color.orange) => new Color(255, 200, 0, 255)
      case c if (c == Color.magenta) => new Color(255, 0, 255, 255)
      case _ => new Color(0,0,0,1.0f)
    }
  }
  
}