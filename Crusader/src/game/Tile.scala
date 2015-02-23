package game

import Output.loadTexture
import Output.drawQuadTex
import TileType.{FLOOR, WALL}
import org.newdawn.slick.opengl.Texture
import Math.sqrt
import Math.abs
import Main._

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
  var explored: Boolean = false
  var label: Int = 0
  var blockMovement: Boolean = TileType.blockMovement(tileType)
  var blockVision: Boolean = TileType.blockVision(tileType)
  var visible: Boolean = false
  
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