package game

import org.newdawn.slick.opengl.Texture
import Output.{loadTexture, drawQuadTex}
import Math.sqrt
import Math.abs

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
  def draw = drawQuadTex(image, x - (Main.player.getX - 16) * 32, 
    y - (Main.player.getY - 8) * 32, image.getImageWidth, image.getImageHeight)
  
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
  
}

/** User's controllable player character */
class Player(playerName: String, startX: Int, startY: Int) extends Object {
  
  var name = playerName
  var description = "the player"
  var x = startX * 32
  var y = startY * 32
  var image = loadTexture("icon")
  var blockMovement = true
  var blockVision = false
  
  var health: Int = 20
  var maxHealth: Int = 20
  var experience: Int = 0
  
  /** Temporary move and attack command */
  def moveOrAttack(newX: Int, newY: Int) = if (!Main.grid.getTile(newX, newY).blockMovement &&
      Main.grid.isWithinGrid(newX, newY) && health > 0) {
    
    var target: Monster = null
    for (monster <- Main.monsterList) {
      if (monster.getX == newX && monster.getY == newY) {
        target = monster
      }
    }
    
    if (target != null) health -= 1
    else if (Main.grid.getTile(newX, newY) == Main.grid.stairs) Main.nextTestMap
    else changePosition(newX, newY)
    
  }
  
  /** Since player is always drawn in the middle of the screen the draw method is overriden */
  override def draw = drawQuadTex(image, 16 * 32, 8 * 32, image.getImageWidth, image.getImageHeight)
  
}

/** All of monsters and npc will be under this class */
class Monster(monsterName: String, monsterdescription: String, startX: Int, startY: Int, 
    monsterImage: String) extends Object {
  
  val rnd = Main.rnd
  var name = monsterName
  var description = monsterdescription
  var x = startX * 32
  var y = startY * 32
  var image = loadTexture(monsterImage)
  var blockMovement = true
  var blockVision = false
  
  /** Temporary method until monster ai is working */
  def turn() {
    move(getX + rnd.nextInt(3)-1, getY + rnd.nextInt(3)-1)
  }
  
  /** Move the object to given location
   *
   * @param newX x-coordinate
   * @param newY y-coordinate
   */
  def move(newX: Int, newY: Int) = if (!Main.grid.getTile(newX, newY).blockMovement &&
      Main.grid.isWithinGrid(newX, newY) && !(newX == Main.player.getX && newY == Main.player.getY)) 
    changePosition(newX, newY)
  
}

/** All of the game's items will be under this trait */
trait Item extends Object {
  
  var price: Int
  var inShop: Boolean
  
}

/** Player usable equipments */
class Equipment(equipmentName: String, equipmentdescription: String, startX: Int, startY: Int, 
    equipmentImage: String, equipmentPrice: Int) extends Item {
  
  var name = equipmentName
  var description = equipmentdescription
  var x = startX * 32
  var y = startY * 32
  var image = loadTexture(equipmentImage)
  var blockMovement = false
  var blockVision = false
  var price = equipmentPrice
  var inShop = false
  
}

/** Player usable consumables */
class Consumable(consumableName: String, consumabledescription: String, startX: Int, startY: Int, 
    consumableImage: String, consumablePrice: Int) extends Item {
  
  var name = consumableName
  var description = consumabledescription
  var x = startX * 32
  var y = startY * 32
  var image = loadTexture(consumableImage)
  var blockMovement = false
  var blockVision = false
  var price = consumablePrice
  var inShop = false
  
}

/** Player usable scrolls */
class Scroll(scrollName: String, scrolldescription: String, startX: Int, startY: Int, 
    scrollImage: String, scrollPrice: Int) extends Item {
  
  var name = scrollName
  var description = scrolldescription
  var x = startX * 32
  var y = startY * 32
  var image = loadTexture(scrollImage)
  var blockMovement = false
  var blockVision = false
  var price = scrollPrice
  var inShop = false
  
}