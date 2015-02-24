package game

import Output.loadTexture
import Output.drawQuadTex
import org.newdawn.slick.opengl.Texture
import Math.sqrt
import Math.abs
import Main._
import collection.mutable.Buffer

  /** Tile represents single tile in the game. The whole map is made of them.
   *
   * @param Xcoord is tile's x-coordinate
   * @param Ycoord is tile's y-coordinate
   * @param tileType is tile's type that it is made of
   */
class Tile(Xcoord: Int, Ycoord: Int, tileType: TileType.Value) {

  private var x = Xcoord * 32
  private var y = Ycoord * 32
  private var image: Texture = TileType.image(tileType)
  private val objectList = Buffer[Object]()
  var explored: Boolean = false
  var label: Int = 0
  var blockMovement: Boolean = TileType.blockMovement(tileType)
  var blockVision: Boolean = TileType.blockVision(tileType)
  var visible: Boolean = false
  
  /** add object to this tile */
  def addObject(obj: Object) = objectList.append(obj)
  
  /** remove object from this tile */
  def removeObject(obj: Object) = objectList.filter(_ == obj) foreach {objectList -= _}
  
  /** getter for object's in this tile */
  def getObjectList = objectList
  
  /** Draw Tile */
  def draw = {
    if (explored)
      drawQuadTex(image, x - (getPlayer.getX - 16) * 32, 
          y - (getPlayer.getY - 8) * 32, image.getImageWidth, image.getImageHeight)
  }
  
  /** Draw Fog */
  def drawFog = {
    if (!visible && explored)
      drawQuadTex(TileType.getFog, x - (getPlayer.getX - 16) * 32, 
          y - (getPlayer.getY - 8) * 32, image.getImageWidth, image.getImageHeight)
  }
  
  /** x and y setters */
  def setX(newX: Int) = x = newX * 32
  def setY(newY: Int) = y = newY * 32
  
  /** getter for tile type */
  def getType() = tileType
  
  /** x and y getters */
  def getX(): Int = x/32
  def getY(): Int = y/32
  
  /** Calculate x difference */
  def xDif(tile: Tile): Int = xDif(tile.getX)
  def xDif(obj: Object): Int = xDif(obj.getX)
  def xDif(x: Int): Int = abs(getX-x).toInt
  
  /** Calculate y difference */
  def yDif(tile: Tile): Int = yDif(tile.getY)
  def yDif(obj: Object): Int = yDif(obj.getY)
  def yDif(y: Int): Int = abs(getY-y).toInt
  
  /** Calculate distance */
  def distance(tile: Tile): Double = distance(tile.getX, tile.getY)
  def distance(obj: Object): Double = distance(obj.getX, obj.getY)
  def distance(x: Int, y: Int): Double = sqrt((xDif(x))*(xDif(x)) + (yDif(y))*(yDif(y)))
  def distance(coordinate: Coordinate): Double = sqrt((xDif(coordinate.getX))*(xDif(coordinate.getX)) + 
      (yDif(coordinate.getY))*(yDif(coordinate.getY)))
  
}

/** There is a defined number of Tiletypes */
object TileType extends Enumeration {

  type Type = Value
  val FLOOR = Value
  val WALL = Value
  val STAIRS = Value
  val DJINNDOORH = Value
  val DJINNDOORV = Value
  val DJINNFLOOR = Value
  val DJINNSLOT = Value
  val DJINNWALL = Value
  
  private val missing = loadTexture("UI/missing")
  private val fog: Texture = loadTexture("Tiles/fog")
  private val grass1 = loadTexture("Tiles/grass1")
  private val wall1 = loadTexture("Tiles/wall1")
  private val tempStairs = loadTexture("tempStairs")
  private val tempDjinnDoorH = loadTexture("tempDjinnDoorH")
  private val tempDjinnDoorV = loadTexture("tempDjinnDoorV")
  private val tempDjinnFloor = loadTexture("tempDjinnFloor")
  private val tempDjinnSlot = loadTexture("tempDjinnSlot")
  private val tempDjinnWall = loadTexture("tempDjinnWall")
  
  /** returns texture of the given tile type */
  def image(tileType: Type): Texture = {
    tileType match {
      case t if (t == FLOOR) => grass1
      case t if (t == WALL) => wall1
      case t if (t == STAIRS) => tempStairs
      case t if (t == DJINNDOORH) => tempDjinnDoorH
      case t if (t == DJINNDOORV) => tempDjinnDoorV
      case t if (t == DJINNFLOOR) => tempDjinnFloor
      case t if (t == DJINNSLOT) => tempDjinnSlot
      case t if (t == DJINNWALL) => tempDjinnWall
      case _ => missing
    }
  }
  
  /** returns true if given tiletype blocks movement */
  def blockMovement(tileType: Type): Boolean = {
    tileType match {
      case t if (t == FLOOR) => false
      case t if (t == WALL) => true
      case t if (t == STAIRS) => false
      case t if (t == DJINNDOORH) => false
      case t if (t == DJINNDOORV) => false
      case t if (t == DJINNFLOOR) => false
      case t if (t == DJINNSLOT) => false
      case t if (t == DJINNWALL) => true
      case _ => true
    }
  }
  
  /** returns true if given tiletype blocks vision */
  def blockVision(tileType: Type): Boolean = {
    tileType match {
      case t if (t == FLOOR) => false
      case t if (t == WALL) => true
      case t if (t == STAIRS) => false
      case t if (t == DJINNDOORH) => true
      case t if (t == DJINNDOORV) => true
      case t if (t == DJINNFLOOR) => false
      case t if (t == DJINNSLOT) => false
      case t if (t == DJINNWALL) => true
      case _ => true
    }
  }
  
  /** fog */
  def getFog = fog

}