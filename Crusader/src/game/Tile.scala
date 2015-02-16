package game

import Output.loadTexture
import Output.drawQuadTex
import TileType.{FLOOR, WALL}
import org.newdawn.slick.opengl.Texture
import Math.sqrt
import Math.abs

  /** Tile represents single tile in the game. The whole map is made of them.
   *

   * @param Xcoord is tile's x-coordinate
   * @param Ycoord is tile's y-coordinate
   * @param tileType is tile's type that it is made of
   */
class Tile(Xcoord: Int, Ycoord: Int, tileType: TileType.Value) {

  var x = Xcoord * 32
  var y = Ycoord * 32
  var explored: Boolean = false
  var label: Int = 0
  var image: Texture = TileType.image(tileType)
  var blockMovement: Boolean = TileType.blockMovement(tileType)
  var blockVision: Boolean = TileType.blockVision(tileType)
  
  /** Draw object */
  def draw = drawQuadTex(image, x - (Main.player.getX - 16) * 32, 
      y - (Main.player.getY - 8) * 32, image.getImageWidth, image.getImageHeight)
  
  /** x and y getters */
  def getX(): Int = x/32
  def getY(): Int = y/32
  
  /** Calculate x difference */
  def xDif(tile: Tile): Int = abs(getX-tile.getX).toInt
  def xDif(obj: Object): Int = abs(getX-obj.getX).toInt
  def xDif(x: Int): Int = abs(getX-x).toInt
  
  /** Calculate y difference */
  def yDif(tile: Tile): Int = abs(getY-tile.getY).toInt
  def yDif(obj: Object): Int = abs(getY-obj.getY).toInt
  def yDif(y: Int): Int = abs(getY-y).toInt
  
  /** Calculate distance */
  def distance(tile: Tile): Int = sqrt((xDif(tile))*(xDif(tile)) + (yDif(tile))*(yDif(tile))).toInt
  def distance(obj: Object): Int = sqrt((xDif(obj))*(xDif(obj)) + (yDif(obj))*(yDif(obj))).toInt
  def distance(x: Int, y: Int): Int = sqrt((xDif(x))*(xDif(x)) + (yDif(y))*(yDif(y))).toInt
  
}