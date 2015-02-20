package game

import Output.loadTexture
import org.newdawn.slick.opengl.Texture

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
  
  private val tempGrass = loadTexture("tempGrass")
  private val tempWall = loadTexture("tempWall")
  private val tempStairs = loadTexture("tempStairs")
  private val missing = loadTexture("UI/missing")
  private val tempDjinnDoorH = loadTexture("tempDjinnDoorH")
  private val tempDjinnDoorV = loadTexture("tempDjinnDoorV")
  private val tempDjinnFloor = loadTexture("tempDjinnFloor")
  private val tempDjinnSlot = loadTexture("tempDjinnSlot")
  private val tempDjinnWall = loadTexture("tempDjinnWall")
  
  /** returns texture of the given tile type */
  def image(tileType: Type): Texture = {
    tileType match {
      case t if (t == FLOOR) => tempGrass
      case t if (t == WALL) => tempWall
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

}