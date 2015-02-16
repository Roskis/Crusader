package game

import Output.loadTexture
import org.newdawn.slick.opengl.Texture

/** There is several Tiletypes */
object TileType extends Enumeration {

  type Type = Value
  val FLOOR = Value
  val WALL = Value
  val STAIRS = Value
  
  val tempGrass = loadTexture("tempGrass")
  val tempWall = loadTexture("tempWall")
  val tempStairs = loadTexture("tempStairs")
  val missing = loadTexture("missing")
  
  def image(tileType: Type): Texture = {
    tileType match {
      case t if (t == FLOOR) => tempGrass
      case t if (t == WALL) => tempWall
      case t if (t == STAIRS) => tempStairs
      case _ => missing
    }
  }
  
  def blockMovement(tileType: Type): Boolean = {
    tileType match {
      case t if (t == FLOOR) => false
      case t if (t == WALL) => true
      case t if (t == STAIRS) => false
      case _ => true
    }
  }
  
  def blockVision(tileType: Type): Boolean = {
    tileType match {
      case t if (t == FLOOR) => false
      case t if (t == WALL) => true
      case t if (t == STAIRS) => false
      case _ => true
    }
  }

}