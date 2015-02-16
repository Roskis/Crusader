package game

import Output.loadTexture
import Output.drawQuadTex
import TileType.{FLOOR, WALL}
import org.newdawn.slick.opengl.Texture
import Math.sqrt
import Math.abs

class Tile(startX: Int, startY: Int, tileType: TileType.Value) {

  var x = startX * 32
  var y = startY * 32
  var explored: Boolean = false
  var label: Int = 0
  var image: Texture = TileType.image(tileType)
  var blockMovement: Boolean = TileType.blockMovement(tileType)
  var blockVision: Boolean = TileType.blockVision(tileType)
  
  def draw = drawQuadTex(image, x - (Main.player.getX - 16) * 32, 
      y - (Main.player.getY - 8) * 32, image.getImageWidth, image.getImageHeight)
  
  def getX(): Int = x/32
  def getY(): Int = y/32
  
  def xDif(tile: Tile): Int = abs(getX-tile.getX).toInt
  def xDif(obj: Object): Int = abs(getX-obj.getX).toInt
  def xDif(x: Int): Int = abs(getX-x).toInt
  
  def yDif(tile: Tile): Int = abs(getY-tile.getY).toInt
  def yDif(obj: Object): Int = abs(getY-obj.getY).toInt
  def yDif(y: Int): Int = abs(getY-y).toInt
  
  def distance(tile: Tile): Int = sqrt((xDif(tile))*(xDif(tile)) + (yDif(tile))*(yDif(tile))).toInt
  def distance(obj: Object): Int = sqrt((xDif(obj))*(xDif(obj)) + (yDif(obj))*(yDif(obj))).toInt
  def distance(x: Int, y: Int): Int = sqrt((xDif(x))*(xDif(x)) + (yDif(y))*(yDif(y))).toInt
  
}