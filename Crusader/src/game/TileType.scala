package game

import Output.loadTexture
import org.newdawn.slick.opengl.Texture

/** There is a defined number of Tiletypes */
object TileType extends Enumeration {

  type Type = Value
  val FLOOR = Value
  val WALL = Value
  val STAIRS = Value
  
  private val tempGrass = loadTexture("tempGrass")
  private val tempWall = loadTexture("tempWall")
  private val tempStairs = loadTexture("tempStairs")
  private val missing = loadTexture("UI/missing")
  
  /** returns texture of the given tile type */
  def image(tileType: Type): Texture = {
    tileType match {
      case t if (t == FLOOR) => tempGrass
      case t if (t == WALL) => tempWall
      case t if (t == STAIRS) => tempStairs
      case _ => missing
    }
  }
  
  /** returns true if given tiletype blocks movement */
  def blockMovement(tileType: Type): Boolean = {
    tileType match {
      case t if (t == FLOOR) => false
      case t if (t == WALL) => true
      case t if (t == STAIRS) => false
      case _ => true
    }
  }
  
  /** returns true if given tiletype blocks vision */
  def blockVision(tileType: Type): Boolean = {
    tileType match {
      case t if (t == FLOOR) => false
      case t if (t == WALL) => true
      case t if (t == STAIRS) => false
      case _ => true
    }
  }

}