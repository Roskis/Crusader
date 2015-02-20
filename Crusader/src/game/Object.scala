package game

import org.newdawn.slick.opengl.Texture
import Output.{loadTexture, drawQuadTex}
import Math.sqrt
import Math.abs

import Main._

/** All of the game's objects will be under this trait */
trait Object {
  
  var name: String
  var description: String
  var x: Int
  var y: Int
  var image: Texture
  var blockMovement: Boolean
  var blockVision: Boolean
  
  /** Draw the object to the screen */
  def draw = drawQuadTex(image, x - (getPlayer.getX - 16) * 32, 
    y - (getPlayer.getY - 8) * 32, image.getImageWidth, image.getImageHeight)
  
  /** Changes the position of this object
   *
   * @param newX x-coordinate
   * @param newY y-coordinate
   */
  def changePosition(newX: Int, newY: Int) = {
    x = newX * 32
    y = newY * 32
  }

  /** x and y getters */
  def getX(): Int = x/32
  def getY(): Int = y/32
  
  /** x and y setters */
  def setX(newX: Int) = x = newX * 32
  def setY(newY: Int) = y = newY * 32
  
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
  def distance(coordinate: Coordinate): Double = sqrt((xDif(coordinate.getX))*(xDif(coordinate.getX)) + 
      (yDif(coordinate.getY))*(yDif(coordinate.getY)))
  
}

/** User's controllable player character */
class Player(playerName: String, startX: Int, startY: Int) extends Object {
  
  var name = playerName
  var description = "the player"
  var x = startX * 32
  var y = startY * 32
  var image = loadTexture("tempPlayer")
  var blockMovement = true
  var blockVision = false
  
  var health: Int = 20
  var maxHealth: Int = 20
  var experience: Int = 0
  
  /** Temporary move and attack command */
  def moveOrAttack(newX: Int, newY: Int) = 
    if (!getGrid.getTile(newX, newY).blockMovement &&
      getGrid.isWithinGrid(newX, newY) && health > 0) {
    
      var target: Monster = null
      for (monster <- getMonsterList) {
        if (monster.getX == newX && monster.getY == newY) {
          target = monster
        }
      }
      
      if (target != null) health -= 1
      else if (getGrid.getTile(newX, newY) == getGrid.getStairs) Main.nextMap
      else changePosition(newX, newY)
    
  }
  
  /** Since player is always drawn in the middle of the screen the draw method is overriden */
  override def draw = drawQuadTex(image, 16 * 32, 8 * 32, image.getImageWidth, image.getImageHeight)
  
}

/** Passive objects are mostly decorative, but might also block some movement */
class PassiveObject(objectName: String, objectDescription: String, startX: Int, startY: Int, 
    objectImage: String) extends Object {
  
  val rnd = getRnd
  var name = objectName
  var description = objectDescription
  var x = startX * 32
  var y = startY * 32
  var image = loadTexture(objectImage)
  var blockMovement = true
  var blockVision = false
  
  getPassiveObjectList.append(this)
  
}

/** All of monsters and npc will be under this class */
class Monster(monsterName: String, monsterDescription: String, startX: Int, startY: Int, 
    monsterImage: String) extends Object {
  
  val rnd = getRnd
  var name = monsterName
  var description = monsterDescription
  var x = startX * 32
  var y = startY * 32
  var image = loadTexture(monsterImage)
  var blockMovement = true
  var blockVision = false
  
  getMonsterList.append(this)
  
  /** Temporary method until monster ai is working */
  def turn() {
    move(getX + rnd.nextInt(3)-1, getY + rnd.nextInt(3)-1)
  }
  
  /** Move the object to given location
   *
   * @param newX x-coordinate
   * @param newY y-coordinate
   */
  def move(newX: Int, newY: Int) = if (!getGrid.getTile(newX, newY).blockMovement &&
      getGrid.isWithinGrid(newX, newY) && !(newX == getPlayer.getX && newY == getPlayer.getY)) 
    changePosition(newX, newY)
  
}

/** All of the game's items will be under this trait */
trait Item extends Object {
  
  var price: Int
  var inShop: Boolean
  
}

/** Player usable equipments */
class Equipment(equipmentName: String, equipmentDescription: String, startX: Int, startY: Int, 
    equipmentImage: String, equipmentPrice: Int) extends Item {
  
  var name = equipmentName
  var description = equipmentDescription
  var x = startX * 32
  var y = startY * 32
  var image = loadTexture(equipmentImage)
  var blockMovement = false
  var blockVision = false
  var price = equipmentPrice
  var inShop = false
  
  getEquipmentList.append(this)
  
}

/** Player usable consumables */
class Consumable(consumableName: String, consumableDescription: String, startX: Int, startY: Int, 
    consumableImage: String, consumablePrice: Int) extends Item {
  
  var name = consumableName
  var description = consumableDescription
  var x = startX * 32
  var y = startY * 32
  var image = loadTexture(consumableImage)
  var blockMovement = false
  var blockVision = false
  var price = consumablePrice
  var inShop = false
  
  getConsumableList.append(this)
  
}

/** Player usable scrolls */
class Scroll(scrollName: String, scrollDescription: String, startX: Int, startY: Int, 
    scrollImage: String, scrollPrice: Int) extends Item {
  
  var name = scrollName
  var description = scrollDescription
  var x = startX * 32
  var y = startY * 32
  var image = loadTexture(scrollImage)
  var blockMovement = false
  var blockVision = false
  var price = scrollPrice
  var inShop = false
  
  getScrollList.append(this)
  
}