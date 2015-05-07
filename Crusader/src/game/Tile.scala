package game

import java.lang.Math.{abs, sqrt}

import scala.collection.mutable.Buffer

import org.newdawn.slick.opengl.Texture

import Main.{getPlayer, getShopVisited, nextMap, getLevel, getGrid}
import Output.drawQuadTex

import Helpers._

  /** Tile represents single tile in the game. The whole map is made of them.
   *
   * @param Xcoord is tile's x-coordinate
   * @param Ycoord is tile's y-coordinate
   * @param tileType is tile's type that it is made of
   */
class Tile(Xcoord: Int, Ycoord: Int, var tileType: TileType.Value) extends Serializable {

  private var x = Xcoord * 32
  private var y = Ycoord * 32
  def image = TileType.image(tileType)
  var objectList = Buffer[Object]()
  var explored: Boolean = false
  var label: Int = 0
  var visible: Boolean = false
  var extraToDraw = Buffer[(Extra.Type, Int, Int)]()
  if (tileType == TileType.DJINNDOORH) addExtra(Extra.DJINNDOORH, 0, 0)
  else if (tileType == TileType.DJINNDOORV) addExtra(Extra.DJINNDOORV, 0, 0)
  
  /** check if this tile or anything on it blocks movement */
  def blockMovement():Boolean = {
    var boo: Boolean = TileType.blockMovement(tileType)
    for (obj <- objectList) if (obj.blockMovement) boo = true
    boo
  }
  
  /** check if this tile or anything on it blocks vision */
  def blockVision():Boolean = {
    var boo: Boolean = TileType.blockVision(tileType)
    for (obj <- objectList) if (obj.blockVision) boo = true
    boo
  }
  
  /** add extra to be drawn with this tile */
  def addExtra(extra: Extra.Type, x: Int, y: Int) = extraToDraw.append((extra, x, y))
  
  /** remove extra from this tile */
  def removeExtra(tex: Texture) = extraToDraw.filter(_._1 == tex) foreach {extraToDraw -= _}
  
  /** add object to this tile */
  def addObject(obj: Object) = {
    objectList.append(obj)
    if (obj.isInstanceOf[Player] && 
        (tileType == TileType.DJINNDOORH || tileType == TileType.DJINNDOORV)) shopGreet
    else if (obj.isInstanceOf[Player] && tileType == TileType.SECRETDOOR) {
      addLog("You have found a secret!")
      tileType = TileType.FLOOR
      getGrid.addGrass(this)
    }
    else if (obj.isInstanceOf[Player] && tileType == TileType.BOSSDOOR2) {
      for (tile <- getGrid.getTiles) if(tile.tileType == TileType.BOSSDOOR1) {
        for (obj <- tile.getObjectList) obj match {
          case o: PassiveObject if (o.pType == PassiveType.GATE1) => {
            Main.getPassiveObjectList.filter(_ == o) foreach {Main.getPassiveObjectList -= _}
            val gate = new PassiveObject("Gate", "TODO", tile.getX, tile.getY, PassiveType.GATE2)
            gate.blockMovement = true
            gate.blockVision = true
          }
          case _ => {}
        }
      }
      tileType = TileType.FLOOR
    }
  }
  
  /** remove object from this tile */
  def removeObject(obj: Object) = objectList.filter(_ == obj) foreach {objectList -= _}
  
  /** getter for object's in this tile */
  def getObjectList = objectList.reverse
  
  /** Draw everything on this tile */
  def drawObjects = {
    if ((tileType == TileType.DJINNFLOOR || tileType == TileType.DJINNWALL) && explored && !visible) {
      drawQuadTex(
          (
            if (getGrid.shopImageNumbers._2 == 1) DjinnWall1
            else if (getGrid.shopImageNumbers._2 == 2) DjinnWall2
            else DjinnWall3
          ), x - (getPlayer.getX - 16) * 32, y - (getPlayer.getY - 8) * 32, image.getImageWidth, image.getImageHeight)
    }
    else if (explored) for (obj <- objectList) obj.draw
  }
  
  def drawExtra = if (explored) {
    for (extra <- extraToDraw) drawQuadTex(Extra.image(extra._1), x - (getPlayer.getX - 16) * 32 + extra._2, 
        y - (getPlayer.getY - 8) * 32 + extra._3, Extra.image(extra._1).getImageWidth, Extra.image(extra._1).getImageHeight)
  }
  
  /** Draw Tile */
  def draw = if (explored) {
    drawQuadTex(image, x - (getPlayer.getX - 16) * 32, y - (getPlayer.getY - 8) * 32, 
        image.getImageWidth, image.getImageHeight)
  }
  
  /** Draw Fog */
  def drawFog = {
    if (!visible && explored && tileType != TileType.DJINNFLOOR && tileType != TileType.DJINNWALL)
      drawQuadTex(TileType.getFog, x - (getPlayer.getX - 16) * 32, y - (getPlayer.getY - 8) * 32, 
      image.getImageWidth, image.getImageHeight)
  }
  
  /** getter for coordinate */
  def getCoordinate: Coordinate = new Coordinate(getX, getY)
  
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

object Extra extends Enumeration {
  type Type = Value
  val GRASS1, GRASS2, GRASS3, FLOWER1, FLOWER2, FLOWER3, FLOWER4, FLOWER5, LAKE1, LAKE2, LAKE3, 
  LAKE4, BIGFLOWER1, BIGFLOWER2, BIGFLOWER3, BIGFLOWER4, BIGFLOWER5, BIGFLOWER6, BUSH1, BUSH2, 
  BUSH3, BUSH4, BUSH5, BUSH6, BUSH7, DJINNDOORH, DJINNDOORV, FLOWER6, FLOWER7, FLOWER8, FLOWER9, 
  FLOWER10, FLOWER11, SMALLROCK1, SMALLROCK2, SMALLROCK3, SMALLROCK4, SMALLROCK5, SMALLROCK6, 
  SMALLROCK7, BIGBUSH1, BIGBUSH2, BIGBUSH3, BIGBUSH4, FERN1, FERN2, FERN3, FERN4, BLOOD1, BLOOD2, 
  BLOOD3, BLOOD4, BLOOD5, BLOOD6, BLOOD7 = Value
  
  def image(extra: Type): Texture = {
    extra match {
      case t if (t == GRASS1) => grass1
      case t if (t == GRASS2) => grass2
      case t if (t == GRASS3) => grass3
      case t if (t == FLOWER1) => flower1
      case t if (t == FLOWER2) => flower2
      case t if (t == FLOWER3) => flower3
      case t if (t == FLOWER4) => flower4
      case t if (t == FLOWER5) => flower5
      case t if (t == FLOWER6) => flower6
      case t if (t == FLOWER7) => flower7
      case t if (t == FLOWER8) => flower8
      case t if (t == FLOWER9) => flower9
      case t if (t == FLOWER10) => flower10
      case t if (t == FLOWER11) => flower11
      case t if (t == LAKE1) => lake1
      case t if (t == LAKE2) => lake2
      case t if (t == LAKE3) => lake3
      case t if (t == LAKE4) => lake4
      case t if (t == BIGFLOWER1) => bigFlower1
      case t if (t == BIGFLOWER2) => bigFlower2
      case t if (t == BIGFLOWER3) => bigFlower3
      case t if (t == BIGFLOWER4) => bigFlower4
      case t if (t == BIGFLOWER5) => bigFlower5
      case t if (t == BIGFLOWER6) => bigFlower6
      case t if (t == BUSH1) => bush1
      case t if (t == BUSH2) => bush2
      case t if (t == BUSH3) => bush3
      case t if (t == BUSH4) => bush4
      case t if (t == BUSH5) => bush5
      case t if (t == BUSH6) => bush6
      case t if (t == BUSH7) => bush7
      case t if (t == DJINNDOORH) => DjinnDoorH
      case t if (t == DJINNDOORV) => DjinnDoorV
      case t if (t == SMALLROCK1) => smallRock1
      case t if (t == SMALLROCK2) => smallRock2
      case t if (t == SMALLROCK3) => smallRock3
      case t if (t == SMALLROCK4) => smallRock4
      case t if (t == SMALLROCK5) => smallRock5
      case t if (t == SMALLROCK6) => smallRock6
      case t if (t == SMALLROCK7) => smallRock7
      case t if (t == BIGBUSH1) => bigBush1
      case t if (t == BIGBUSH2) => bigBush2
      case t if (t == BIGBUSH3) => bigBush3
      case t if (t == BIGBUSH4) => bigBush4
      case t if (t == FERN1) => fern1
      case t if (t == FERN2) => fern2
      case t if (t == FERN3) => fern3
      case t if (t == FERN4) => fern4
      case t if (t == BLOOD1) => blood1
      case t if (t == BLOOD2) => blood2
      case t if (t == BLOOD3) => blood3
      case t if (t == BLOOD4) => blood4
      case t if (t == BLOOD5) => blood5
      case t if (t == BLOOD6) => blood6
      case t if (t == BLOOD7) => blood7
      case _ => missing
    }
  }
}

/** There is a defined number of Tiletypes */
object TileType extends Enumeration {

  type Type = Value
  val FLOOR, WALL, DJINNDOORH, DJINNDOORV, DJINNFLOOR, DJINNWALL, SECRETDOOR, BOSSDOOR1, BOSSDOOR2 = Value
  private val rnd = Main.getRnd
  
  /** returns texture of the given tile type */
  def image(tileType: Type): Texture = {
    tileType match {
      case t if (t == FLOOR) => floorEp1
      case t if (t == WALL) => wall1
      case t if (t == DJINNFLOOR || t == DJINNDOORH || t == DJINNDOORV) => {
        if (getGrid.shopImageNumbers._3 == 1) DjinnFloor1
        else if (getGrid.shopImageNumbers._3 == 2) DjinnFloor2
        else DjinnFloor3
      }
      case t if (t == DJINNWALL) => {
        if (getGrid.shopImageNumbers._2 == 1) DjinnWall1
        else if (getGrid.shopImageNumbers._2 == 2) DjinnWall2
        else DjinnWall3
      }
      case t if (t == SECRETDOOR) => wall1
      case t if (t == BOSSDOOR1) => floorEp1
      case t if (t == BOSSDOOR2) => floorEp1
      case _ => missing
    }
  }
  
  /** returns true if given tiletype blocks movement */
  def blockMovement(tileType: Type): Boolean = {
    tileType match {
      case t if (t == FLOOR) => false
      case t if (t == WALL) => true
      case t if (t == DJINNDOORH) => false
      case t if (t == DJINNDOORV) => false
      case t if (t == DJINNFLOOR) => false
      case t if (t == DJINNWALL) => true
      case t if (t == SECRETDOOR) => false
      case t if (t == BOSSDOOR1) => false
      case t if (t == BOSSDOOR2) => false
      case _ => true
    }
  }
  
  /** returns true if given tiletype blocks vision */
  def blockVision(tileType: Type): Boolean = {
    tileType match {
      case t if (t == FLOOR) => false
      case t if (t == WALL) => true
      case t if (t == DJINNDOORH) => true
      case t if (t == DJINNDOORV) => true
      case t if (t == DJINNFLOOR) => false
      case t if (t == DJINNWALL) => true
      case t if (t == SECRETDOOR) => true
      case t if (t == BOSSDOOR1) => false
      case t if (t == BOSSDOOR2) => false
      case _ => true
    }
  }
  
  /** fog */
  def getFog = fog

}