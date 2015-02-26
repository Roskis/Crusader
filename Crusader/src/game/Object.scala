package game

import org.newdawn.slick.opengl.Texture
import Output.{loadTexture, drawQuadTex, addLog}
import Math.sqrt
import Math.abs
import Direction._
import Main._
import collection.mutable.Buffer

/** All of the game's objects will be under this trait */
trait Object {
  
  val rnd: scala.util.Random
  var name: String
  var description: String
  var x: Int
  var y: Int
  var image: Texture
  var blockMovement: Boolean
  var blockVision: Boolean
  var isMonster: Boolean
  
  /** Simple way to roll multiple dice */
  def roll(amount: Int, number: Int): Int = {
    var num: Int = 0
    for (dice <- (0 until amount)) num += roll(number)
    num
  }
  
  /** Simple way to roll one dice */
  def roll(number: Int): Int = {
    getRnd.nextInt(number) + 1
  }
  
  /** dummy methods */
  def pickUp {}
  def dodge = 0
  def takeDamage(a: Int, b: Double, c: Object) {}
  
  /** Add object to the tile it is on */
  def init() = if (getGrid.isWithinGrid(getX, getY)) getGrid.getTile(getX, getY).addObject(this)
  
  /** Draw the object to the screen */
  def draw = drawQuadTex(image, x - (getPlayer.getX - 16) * 32, 
    y - (getPlayer.getY - 8) * 32, image.getImageWidth, image.getImageHeight)
  
  /** block current tile's vision */
  def blockVisionForTile() = {
    getGrid.getTile(getX, getY).blockVision = true
  }
    
  /** unblock current tile's vision */
  def unblockVisionForTile = {
    getGrid.getTile(getX, getY).blockVision = false
  }
  
  /** Check if this object is only visionblocker in this tile */
  def onlyVisionBlocker(): Boolean = {
    var boo: Boolean = true
    if (getGrid.isWithinGrid(getX, getY)) {
      for (obj <- getGrid.getTile(getX, getY).getObjectList) if (obj.blockVision && obj.getX == getX && obj.getY == getY) boo = false
    }
    boo
  }
  
  /** Changes the position of this object
   *
   * @param newX x-coordinate
   * @param newY y-coordinate
   */
  def changePosition(newX: Int, newY: Int) = {
    if (blockVision && onlyVisionBlocker) unblockVisionForTile
    if (getGrid.isWithinGrid(getX, getY)) getGrid.getTile(getX, getY).removeObject(this)
    x = newX * 32
    y = newY * 32
    if (blockVision) blockVisionForTile
    if (getGrid.isWithinGrid(getX, getY)) getGrid.getTile(getX, getY).addObject(this)
  }

  /** x and y getters */
  def getX(): Int = x/32
  def getY(): Int = y/32
  
  /** x and y setters */
  def setX(newX: Int) = x = newX * 32
  def setY(newY: Int) = y = newY * 32
  
  /** Calculate x difference */
  def xDif(tile: Tile): Int = xDif(tile.getX)
  def xDif(obj: Object): Int = xDif(obj.getX)
  def xDif(x: Int): Int = abs(getX-x).toInt
  
  /** Calculate y difference */
  def yDif(tile: Tile): Int = yDif(tile.getY)
  def yDif(obj: Object): Int = yDif(obj.getY)
  def yDif(y: Int): Int = abs(getY-y).toInt
  
  /** Calculate distance */
  def distance(tile: Tile): Int = distance(tile.getX, tile.getY)
  def distance(obj: Object): Int = distance(obj.getX, obj.getY)
  def distance(x: Int, y: Int): Int = sqrt((xDif(x))*(xDif(x)) + (yDif(y))*(yDif(y))).toInt
  def distance(coordinate: Coordinate): Double = sqrt((xDif(coordinate.getX))*(xDif(coordinate.getX)) + 
      (yDif(coordinate.getY))*(yDif(coordinate.getY)))
  
}

/** User's controllable player character */
class Player(playerName: String, startX: Int, startY: Int) extends Object {
  
  val rnd = getRnd
  var name = playerName
  var description = "TODO"
  var x = startX * 32
  var y = startY * 32
  var image = loadTexture("Player/humanBase")
  var blockMovement = true
  var blockVision = false
  var isMonster = false
  
  var viewRadius = 10
  var health: Double = 20
  var experience: Double = 0
  var gold: Double = 0
  var piety: Double = 0
  
  var zeal: Int = 0
  var humility: Int = 0
  var temperance: Int = 0
  var kindness: Int = 0
  var patience: Int = 0
  var charity: Int = 0
  var diligence: Int = 0
  
  var slotWeapon: Equipment = new Equipment(-100, -100, EquipmentType.KNIFE, true)
  var slotArmor: Equipment = new Equipment(-100, -100, EquipmentType.ROBES, true)
  var slotShield: Equipment = null
  var slotRing: Equipment = null
  var slotAmulet: Equipment = null
  var slotItem: Equipment = null
  
  val grave = loadTexture("Environment/grave")
  
  /** modifier applied to all experience gained */
  def xpModifier: Double = 1+0.1*diligence
  
  /** modifier applied to all gold gained */
  def goldModifier: Double = 1+0.1*diligence
  
  /** modifier applied to all piety gained */
  def pietyModifier: Double = 1+0.1*charity
  
  /** Player's maximum health */
  def maxHealth: Int = 20+8*kindness
  
  /** Title of player */
  def title: String = "the Holy"
  
  /** Total level of player */
  def totalLevel(): Int = zeal + humility + temperance + kindness + patience + charity + diligence
  
  /** Return smallest level xp requirement */
  def smallestLevel(): Int = {
    var skills = List(zeal, humility, temperance, kindness, patience, charity, diligence)
    skills = skills.sortWith(_ < _)
    xpNeededForLevel(skills(0) + 1)
  }
  
  /** Method to deal damage to monsters */
  def attack(target: Object) {
    if (rnd.nextInt(100) <= accuracy - target.dodge) {
      target.takeDamage(damage(rnd.nextInt(100) <= crit), armorPierce, this)
    }
    else target.takeDamage(smallestDamage, armorPierce, this)
  }
  
  /** Damage is based on player's weapon */
  def damage(crit: Boolean): Int = {
    val weapondmg = roll(slotWeapon.damage._1, slotWeapon.damage._2) + slotWeapon.damage._3
    if (crit) weapondmg + roll(zeal+2) + roll(zeal+2)
    else weapondmg + roll(zeal+2)
  }
  
  /** Return small damage */
  def smallestDamage: Int = {
    var num = 0
    val zealroll = roll(zeal+2)
    val weaponroll = roll(slotWeapon.damage._1, slotWeapon.damage._2) + slotWeapon.damage._3
    if (rnd.nextInt(4) != 0) if (zealroll < weaponroll) num = zealroll else num = weaponroll
    num
  }
  
  override def takeDamage(damage: Int, armorPierce: Double, attacker: Object) = {
    var effectiveArmor = armor
    if (rnd.nextInt(100) <= blockChance) effectiveArmor += shieldArmor
    if (effectiveArmor < 0) effectiveArmor = 0
    var effectiveDamage = (damage - effectiveArmor)
    if (effectiveDamage < 0) effectiveDamage = 0
    health -= effectiveDamage
    addLog(attacker.name + " deals " + effectiveDamage.toString + " damage to " + name)
  }
  
  def armor: Double = {
    val patienceBonus: Double = {
      if (patience > 7) 2
      else if (patience > 5) 1.5
      else if (patience > 3) 1
      else if (patience > 1) 0.5
      else 0
    }
    slotArmor.armor + patienceBonus
  }
  
  def armorPierce: Double = if (slotWeapon != null) slotWeapon.armorPiercing else 0
  
  /** Critical chance of weapon used */
  def crit: Int = if (slotWeapon != null) slotWeapon.critChance else 2
  
  /** Block chance of shield used */
  def blockChance: Int = if (slotShield != null) slotShield.blockChance else 0
  
  /** Shield Armor */
  def shieldArmor: Double = if (slotShield != null) slotShield.armor else 0
  
  /** Total weight of items after patience bonus */
  def totalWeight: Int = {
    var num: Int = 0
    if (slotWeapon != null) num += slotWeapon.weight
    if (slotArmor != null) num += slotArmor.weight
    if (slotShield != null) num += slotShield.weight
    if (slotRing != null) num += slotRing.weight
    if (slotAmulet != null) num += slotAmulet.weight
    if (slotItem != null) num += slotItem.weight
    ((1 - (0.05*patience)).toInt * num)
  }
  
  /** Return dodge of player */
  override def dodge: Int = 100 - totalWeight + humility*2
  
  /** Return accuracy of player */
  def accuracy: Int = if (slotWeapon != null) slotWeapon.accuracy + temperance*2 else 100 + temperance*2
  
  /** Return needed amount of experience to level up */
  def xpNeededForLevel(level: Int): Int = {
    level match {
      case l if (l == 1) => 10
      case l if (l == 2) => 25
      case l if (l == 3) => 85
      case l if (l == 4) => 225
      case l if (l == 5) => 750
      case l if (l == 6) => 1800
      case l if (l == 7) => 3000
      case l if (l == 8) => 5000
      case l if (l == 9) => 8000
      case l if (l == 10) => 15000
      case _ => 10
    }
  }
  
  /** Temporary move and attack command */
  def moveOrAttack(direction: Direction.Value) = {
    val newX = getCoordinates(direction, getX, getY).getX
    val newY = getCoordinates(direction, getX, getY).getY
    if (getGrid.isWithinGrid(newX, newY) && !getGrid.getTile(newX, newY).blockMovement) {
      var target: Object = null
      for (obj <- getGrid.getTile(newX, newY).getObjectList) {
        obj match {
          case _: Monster => target = obj
          case _: Item => obj.pickUp
          case _ =>
        }
      }
      
      if (target != null) attack(target)
      else if (getGrid.getTile(newX, newY) == getGrid.getStairs) Main.nextMap
      else changePosition(newX, newY)
    }
  }
  
  /** Since player is always drawn in the middle of the screen the draw method is overriden */
  override def draw = {
    if (health > 0) {
      drawQuadTex(image, 16 * 32, 8 * 32, image.getImageWidth, image.getImageHeight)
      if (slotWeapon != null) drawQuadTex(slotWeapon.imageEquipped, 16 * 32, 8 * 32, slotWeapon.imageEquipped.getImageWidth, slotWeapon.imageEquipped.getImageHeight)
      if (slotArmor != null) drawQuadTex(slotArmor.imageEquipped, 16 * 32, 8 * 32, slotArmor.imageEquipped.getImageWidth, slotArmor.imageEquipped.getImageHeight)
      if (slotShield != null) drawQuadTex(slotShield.imageEquipped, 16 * 32, 8 * 32, slotShield.imageEquipped.getImageWidth, slotShield.imageEquipped.getImageHeight)
      if (slotRing != null) drawQuadTex(slotRing.imageEquipped, 16 * 32, 8 * 32, slotRing.imageEquipped.getImageWidth, slotRing.imageEquipped.getImageHeight)
      if (slotAmulet != null) drawQuadTex(slotAmulet.imageEquipped, 16 * 32, 8 * 32, slotAmulet.imageEquipped.getImageWidth, slotAmulet.imageEquipped.getImageHeight)
      if (slotItem != null) drawQuadTex(slotItem.imageEquipped, 16 * 32, 8 * 32, slotItem.imageEquipped.getImageWidth, slotItem.imageEquipped.getImageHeight)
      /**
      drawQuadTex(robesE, 16 * 32, 8 * 32, robesE.getImageWidth, robesE.getImageHeight)
      drawQuadTex(steelSwordE, 16 * 32, 8 * 32, steelSwordE.getImageWidth, steelSwordE.getImageHeight)
      drawQuadTex(woodenShieldE, 16 * 32, 8 * 32, woodenShieldE.getImageWidth, woodenShieldE.getImageHeight)
      */
    }
    else drawQuadTex(grave, 16 * 32, 8 * 32, grave.getImageWidth, grave.getImageHeight)
  }
  
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
  var isMonster = false
  
  init
  getPassiveObjectList.append(this)
  
}

/** All of monsters and npc will be under this class */
class Monster(startX: Int, startY: Int, monsterType: MonsterType.Value) extends Object {
  
  val mType = monsterType
  val rnd = getRnd
  var name = MonsterType.name(mType)
  var description = MonsterType.description(mType)
  var x = startX * 32
  var y = startY * 32
  var image = MonsterType.image(mType)
  var blockMovement = true
  var blockVision = false
  var isMonster = true
  
  var health: Double = MonsterType.maxHP(mType)
  var armor: Double = MonsterType.armor(mType)
  var damage = MonsterType.damage(mType)
  var accuracy = MonsterType.accuracy(mType)
  var ap: Double = MonsterType.armorPierce(mType)
  var crit = MonsterType.criticalChance(mType)
  var experience = MonsterType.experience(mType)
  var gold = MonsterType.gold(mType)
  var piety = MonsterType.piety(mType)
  override def dodge = MonsterType.dodge(mType)
  var blockChance = 0
  var shieldArmor = 0
  
  init
  getMonsterList.append(this)
  
  /** When monster dies this method is called */
  def kill() {
    getMonsterList.filter(_ == this) foreach {getMonsterList -= _}
    getGrid.getTile(getX, getY).removeObject(this)
    x = -100
    y = -100
    addLog(name + " dies")
    getPlayer.experience += experience
    getPlayer.gold += gold
    getPlayer.piety += piety
  }
  
  /** Takes damage from attack */
  override def takeDamage(damage: Int, armorPierce: Double, attacker: Object) = {
    var effectiveArmor = armor
    if (rnd.nextInt(100) <= blockChance) effectiveArmor += shieldArmor
    if (effectiveArmor < 0) effectiveArmor = 0
    var effectiveDamage = (damage - effectiveArmor)
    if (effectiveDamage < 0) effectiveDamage = 0
    health -= effectiveDamage
    addLog(attacker.name + " deals " + effectiveDamage.toString + " damage to " + name)
    if (health < 0) kill
  }
  
  /** Method to deal damage to player */
  def attack(target: Object) {
    if (rnd.nextInt(100) <= accuracy - target.dodge) {
      target.takeDamage(damageroll(rnd.nextInt(100) <= crit), ap, this)
    }
    else target.takeDamage(smallDamage, ap, this)
  }
  
  /** Damage is based on player's weapon */
  def damageroll(crit: Boolean): Int = {
    val dmg = roll(damage._1, damage._2) + damage._3
    if (crit) dmg + dmg
    else dmg
  }
  
  /** Return small damage */
  def smallDamage: Int = {
    val dmg = roll(damage._1, damage._2) + damage._3
    if (rnd.nextInt(4) != 0) (dmg/2).toInt else 0
  }
  
  /** Temporary method until monster ai is working */
  def turn() {
    if (distance(getPlayer) > 8) move(randomDirection(8))
    else if (distance(getPlayer) < 2) attack(getPlayer)
    else move(getGrid.line(getX, getY, getPlayer.getX, getPlayer.getY)(1))
  }
  
  /** Move monster to given coordinate */
  def move(coord: Coordinate): Unit = {
    if (getGrid.isWithinGrid(coord.getX, coord.getY)) {
      var boo = true
      for (obj <- getGrid.getTile(coord.getX, coord.getY).getObjectList) if (obj.isMonster == true) boo = false
      if (distance(coord) < 2 && !getGrid.getTile(coord.getX, coord.getY).blockMovement && boo) {
        changePosition(coord.getX, coord.getY)
      }
    }
  }
  
  /** Move the object to given direction */
  def move(direction: Direction.Value): Unit =  move(new Coordinate(
      getCoordinates(direction, getX, getY).getX, getCoordinates(direction, getX, getY).getY))
  
  /** Draw the object to the screen */
  override def draw = {
    if (getGrid.getTile(getX, getY).visible && getGrid.getTile(getX, getY).explored) 
      drawQuadTex(image, x - (getPlayer.getX - 16) * 32, 
          y - (getPlayer.getY - 8) * 32, image.getImageWidth, image.getImageHeight)
  }
  
}

object MonsterType extends Enumeration {

  type Type = Value
  val BAT = Value
  val SNAKE = Value
  val SPIDER = Value
  val GOBLINA = Value
  val GOBLINB = Value
  val HOUND = Value
  val LIZARDA = Value
  val LIZARDB = Value
  val LIZARDC = Value
  val CROCODILE = Value
  
  private val missing = loadTexture("UI/missing")
  private val bat = loadTexture("Monsters/bat1")
  private val snake = loadTexture("Monsters/snake1")
  private val spider = loadTexture("Monsters/spider1")
  private val goblina = loadTexture("Monsters/goblina1")
  private val goblinb = loadTexture("Monsters/goblinb1")
  private val hound = loadTexture("Monsters/hound1")
  private val lizarda = loadTexture("Monsters/lizarda1")
  private val lizardb = loadTexture("Monsters/lizardb1")
  private val lizardc = loadTexture("Monsters/lizardc1")
  private val crocodile = loadTexture("Monsters/crocodile1")
  
  //* Returns Buffer containing all of the spawnable monsters */
  def monstersForLevel(level: Int): Buffer[Value] = {
    var list = Buffer[Value]()
    level match {
      case l if (l == 1) => {
        list += BAT
        list += SNAKE
        list += SPIDER
      }
      case l if (l == 2) => {
        list += BAT
        list += SNAKE
        list += SPIDER
        list += GOBLINA
        list += GOBLINB
        list += HOUND
      }
      case l if (l == 3) => {
        list += BAT
        list += SNAKE
        list += SPIDER
        list += GOBLINA
        list += GOBLINB
        list += HOUND
        list += LIZARDA
        list += LIZARDB
        list += LIZARDC
        list += CROCODILE
      }
      case l if (l == 4) => {
        list += GOBLINA
        list += GOBLINB
        list += HOUND
        list += LIZARDA
        list += LIZARDB
        list += LIZARDC
        list += CROCODILE
      }
      case _ =>
    }
    list
  }
  
  /** returns texture of the given monster */
  def image(MonsterType: Type): Texture = {
    MonsterType match {
      case t if (t == BAT) => bat
      case t if (t == SNAKE) => snake
      case t if (t == SPIDER) => spider
      case t if (t == GOBLINA) => goblina
      case t if (t == GOBLINB) => goblinb
      case t if (t == HOUND) => hound
      case t if (t == LIZARDA) => lizarda
      case t if (t == LIZARDB) => lizardb
      case t if (t == LIZARDC) => lizardc
      case t if (t == CROCODILE) => crocodile
      case _ => missing
    }
  }
  
  /** returns max health of the given monster */
  def maxHP(MonsterType: Type): Int = {
    MonsterType match {
      case t if (t == BAT) => 2
      case t if (t == SNAKE) => 5
      case t if (t == SPIDER) => 5
      case t if (t == GOBLINA) => 15
      case t if (t == GOBLINB) => 15
      case t if (t == HOUND) => 10
      case t if (t == LIZARDA) => 15
      case t if (t == LIZARDB) => 15
      case t if (t == LIZARDC) => 15
      case t if (t == CROCODILE) => 20
      case _ => 0
    }
  }
  
  /** returns damage of the given monster 
   * 
   * Tuple3 includes number of dices, their number of eyes and additional flat bonus 
   * (num of dices, eyes, flat). Examples 2d3+5 = (2, 3, 5) and 1d4 = (1, 4, 0).
   *  */
  def damage(MonsterType: Type): Tuple3[Int, Int, Int] = {
    MonsterType match {
      case t if (t == BAT) => (1, 2, 0)
      case t if (t == SNAKE) => (1, 2, 0)
      case t if (t == SPIDER) => (1, 2, 0)
      case t if (t == GOBLINA) => (1, 2, 0)
      case t if (t == GOBLINB) => (1, 3, 0)
      case t if (t == HOUND) => (1, 3, 0)
      case t if (t == LIZARDA) => (1, 3, 0)
      case t if (t == LIZARDB) => (1, 4, 0)
      case t if (t == LIZARDC) => (1, 4, 0)
      case t if (t == CROCODILE) => (1, 5, 0)
      case _ => (0, 0, 0)
    }
  }
  
  /** returns armor of the given monster */
  def armor(MonsterType: Type): Double = {
    MonsterType match {
      case t if (t == BAT) => 0
      case t if (t == SNAKE) => 0
      case t if (t == SPIDER) => 0
      case t if (t == GOBLINA) => 1
      case t if (t == GOBLINB) => 1.5
      case t if (t == HOUND) => 0
      case t if (t == LIZARDA) => 2
      case t if (t == LIZARDB) => 1
      case t if (t == LIZARDC) => 0
      case t if (t == CROCODILE) => 2
      case _ => 0
    }
  }
  
  /** returns accuracy of the given monster */
  def accuracy(MonsterType: Type): Int = {
    MonsterType match {
      case t if (t == BAT) => 95
      case t if (t == SNAKE) => 90
      case t if (t == SPIDER) => 90
      case t if (t == GOBLINA) => 85
      case t if (t == GOBLINB) => 85
      case t if (t == HOUND) => 90
      case t if (t == LIZARDA) => 90
      case t if (t == LIZARDB) => 85
      case t if (t == LIZARDC) => 75
      case t if (t == CROCODILE) => 80
      case _ => 0
    }
  }
  
  /** returns critical chance of the given monster */
  def criticalChance(MonsterType: Type): Int = {
    MonsterType match {
      case t if (t == BAT) => 2
      case t if (t == SNAKE) => 4
      case t if (t == SPIDER) => 2
      case t if (t == GOBLINA) => 4
      case t if (t == GOBLINB) => 5
      case t if (t == HOUND) => 6
      case t if (t == LIZARDA) => 5
      case t if (t == LIZARDB) => 5
      case t if (t == LIZARDC) => 10
      case t if (t == CROCODILE) => 2
      case _ => 0
    }
  }
  
  /** returns dodge chance of the given monster */
  def dodge(MonsterType: Type): Int = {
    MonsterType match {
      case t if (t == BAT) => 30
      case t if (t == SNAKE) => 10
      case t if (t == SPIDER) => 15
      case t if (t == GOBLINA) => 10
      case t if (t == GOBLINB) => 12
      case t if (t == HOUND) => 10
      case t if (t == LIZARDA) => 8
      case t if (t == LIZARDB) => 8
      case t if (t == LIZARDC) => 10
      case t if (t == CROCODILE) => 0
      case _ => 0
    }
  }
  
  /** returns armor pierce of the given monster */
  def armorPierce(MonsterType: Type): Int = {
    MonsterType match {
      case t if (t == BAT) => 0
      case t if (t == SNAKE) => 0
      case t if (t == SPIDER) => 0
      case t if (t == GOBLINA) => 0
      case t if (t == GOBLINB) => 1
      case t if (t == HOUND) => 0
      case t if (t == LIZARDA) => 2
      case t if (t == LIZARDB) => 1
      case t if (t == LIZARDC) => 0
      case t if (t == CROCODILE) => 2
      case _ => 0
    }
  }
  
  /** returns gold of the given monster */
  def gold(MonsterType: Type): Int = {
    MonsterType match {
      case t if (t == BAT) => 1
      case t if (t == SNAKE) => 3
      case t if (t == SPIDER) => 2
      case t if (t == GOBLINA) => 10
      case t if (t == GOBLINB) => 15
      case t if (t == HOUND) => 5
      case t if (t == LIZARDA) => 10
      case t if (t == LIZARDB) => 10
      case t if (t == LIZARDC) => 10
      case t if (t == CROCODILE) => 20
      case _ => 0
    }
  }
  
  /** returns experience of the given monster */
  def experience(MonsterType: Type): Int = {
    MonsterType match {
      case t if (t == BAT) => 1
      case t if (t == SNAKE) => 3
      case t if (t == SPIDER) => 2
      case t if (t == GOBLINA) => 10
      case t if (t == GOBLINB) => 15
      case t if (t == HOUND) => 5
      case t if (t == LIZARDA) => 15
      case t if (t == LIZARDB) => 15
      case t if (t == LIZARDC) => 15
      case t if (t == CROCODILE) => 20
      case _ => 0
    }
  }
  
  /** returns piety of the given monster */
  def piety(MonsterType: Type): Int = {
    MonsterType match {
      case t if (t == BAT) => 1
      case t if (t == SNAKE) => 2
      case t if (t == SPIDER) => 4
      case t if (t == GOBLINA) => 10
      case t if (t == GOBLINB) => 15
      case t if (t == HOUND) => 3
      case t if (t == LIZARDA) => 15
      case t if (t == LIZARDB) => 15
      case t if (t == LIZARDC) => 15
      case t if (t == CROCODILE) => 0
      case _ => 0
    }
  }
  
  /** returns name of the given monster */
  def name(MonsterType: Type): String = {
    MonsterType match {
      case t if (t == BAT) => "bat"
      case t if (t == SNAKE) => "Snake"
      case t if (t == SPIDER) => "Spider"
      case t if (t == GOBLINA) => "Goblin"
      case t if (t == GOBLINB) => "Goblin"
      case t if (t == HOUND) => "Hound"
      case t if (t == LIZARDA) => "Lizard"
      case t if (t == LIZARDB) => "Lizard"
      case t if (t == LIZARDC) => "Lizard"
      case t if (t == CROCODILE) => "Crocodile"
      case _ => "Unknown monster name"
    }
  }
  
  /** returns description of the given monster */
  def description(MonsterType: Type): String = {
    MonsterType match {
      case t if (t == BAT) => "TODO"
      case t if (t == SNAKE) => "TODO"
      case t if (t == SPIDER) => "TODO"
      case t if (t == GOBLINA) => "TODO"
      case t if (t == GOBLINB) => "TODO"
      case t if (t == HOUND) => "TODO"
      case t if (t == LIZARDA) => "TODO"
      case t if (t == LIZARDB) => "TODO"
      case t if (t == LIZARDC) => "TODO"
      case t if (t == CROCODILE) => "TODO"
      case _ => "Unknown monster name"
    }
  }
  
}

/** All of the game's items will be under this trait */
trait Item extends Object {
  
  var price: Int
  var inShop: Boolean
  var equipped: Boolean
  
}

/** Player usable equipments */
class Equipment(startX: Int, startY: Int, equipmentType: EquipmentType.Value, isEquipped: Boolean) extends Item {
  
  val rnd = getRnd
  val eType = equipmentType
  var name = EquipmentType.name(eType)
  var description = EquipmentType.description(eType)
  var x = startX * 32
  var y = startY * 32
  var image = EquipmentType.imageGround(eType)
  var imageEquipped = EquipmentType.imageEquipped(eType)
  var blockMovement = false
  var blockVision = false
  var isMonster = false
  var price = EquipmentType.price(eType)
  var inShop = false
  val armor = EquipmentType.armor(eType)
  val weight = EquipmentType.weight(eType)
  val blockChance = EquipmentType.blockChance(eType)
  val damage = EquipmentType.damage(eType)
  val armorPiercing = EquipmentType.armorPiercing(eType)
  val accuracy = EquipmentType.accuracy(eType)
  val critChance = EquipmentType.critChance(eType)
  var equipped: Boolean = isEquipped
  
  if (!equipped) {
    init
    getEquipmentList.append(this)
  }
  
  /** unequip item */
  def unequip {
    if (getPlayer.slotWeapon != null && EquipmentType.slot(getPlayer.slotWeapon.eType) == EquipmentType.slot(equipmentType)) getPlayer.slotWeapon = null
    else if (getPlayer.slotArmor != null && EquipmentType.slot(getPlayer.slotArmor.eType) == EquipmentType.slot(equipmentType)) getPlayer.slotArmor = null
    else if (getPlayer.slotShield != null && EquipmentType.slot(getPlayer.slotShield.eType) == EquipmentType.slot(equipmentType)) getPlayer.slotShield = null
    else if (getPlayer.slotRing != null && EquipmentType.slot(getPlayer.slotRing.eType) == EquipmentType.slot(equipmentType)) getPlayer.slotRing = null
    else if (getPlayer.slotAmulet != null && EquipmentType.slot(getPlayer.slotAmulet.eType) == EquipmentType.slot(equipmentType)) getPlayer.slotAmulet = null
    else if (getPlayer.slotItem != null && EquipmentType.slot(getPlayer.slotItem.eType) == EquipmentType.slot(equipmentType)) getPlayer.slotItem = null
    equipped = false
    x = getPlayer.getX * 32
    y = getPlayer.getY * 32
    init
    getEquipmentList.append(this)
  }
  
  /** equip item */
  def equip {
    if (EquipmentType.slot(equipmentType) == "weapon") getPlayer.slotWeapon = this
    else if (EquipmentType.slot(equipmentType) == "armor") getPlayer.slotArmor = this
    else if (EquipmentType.slot(equipmentType) == "shield") getPlayer.slotShield = this
    else if (EquipmentType.slot(equipmentType) == "ring") getPlayer.slotRing = this
    else if (EquipmentType.slot(equipmentType) == "amulet") getPlayer.slotAmulet = this
    else if (EquipmentType.slot(equipmentType) == "item") getPlayer.slotItem = this
    getEquipmentList.filter(_ == this) foreach {getEquipmentList -= _}
    getGrid.getTile(getX, getY).removeObject(this)
    equipped = true
    x = -100
    y = -100
  }
  
  /** Pick up */
  override def pickUp = {
    EquipmentType.slot(equipmentType) match {
      case s if (s == "weapon") => {
        if (getPlayer.slotWeapon != null) getPlayer.slotWeapon.unequip
        equip
      }
      case s if (s == "armor") => {
        if (getPlayer.slotArmor != null) getPlayer.slotArmor.unequip
        equip
      }
      case s if (s == "shield") => {
        if (getPlayer.slotShield != null) getPlayer.slotShield.unequip
        equip
      }
      case s if (s == "ring") => {
        if (getPlayer.slotRing != null) getPlayer.slotRing.unequip
        equip
      }
      case s if (s == "amulet") => {
        if (getPlayer.slotAmulet != null) getPlayer.slotAmulet.unequip
        equip
      }
      case s if (s == "item") => {
        if (getPlayer.slotItem != null) getPlayer.slotItem.unequip
        equip
      }
      case _ =>
    }
  }
  
}

object EquipmentType extends Enumeration {

  type Type = Value
  val KNIFE = Value
  val ROBES = Value
  val IRONARMOR = Value
  val STEELSWORD = Value
  val WOODENSHIELD = Value
  
  private val missing = loadTexture("UI/missing")
  private val knifeG = loadTexture("Items/knifeG")
  private val knifeE = loadTexture("Player/knifeE")
  private val robesG = loadTexture("Items/robesG")
  private val robesE = loadTexture("Player/robesE")
  private val ironArmorG = loadTexture("Items/ironArmorG")
  private val ironArmorE = loadTexture("Player/ironArmorE")
  private val steelSwordG = loadTexture("Items/steelSwordG")
  private val steelSwordE = loadTexture("Player/steelSwordE")
  private val woodenShieldG = loadTexture("Items/woodenShieldG")
  private val woodenShieldE = loadTexture("Player/woodenShieldE")
  
  /** returns ground texture of the given equipment */
  def slot(EquipmentType: Type): String = {
    EquipmentType match {
      case t if (t == KNIFE) => "weapon"
      case t if (t == ROBES) => "armor"
      case t if (t == IRONARMOR) => "armor"
      case t if (t == STEELSWORD) => "weapon"
      case t if (t == WOODENSHIELD) => "shield"
      case _ => ""
    }
  }
  
  /** returns ground texture of the given equipment */
  def imageGround(EquipmentType: Type): Texture = {
    EquipmentType match {
      case t if (t == KNIFE) => knifeG
      case t if (t == ROBES) => robesG
      case t if (t == IRONARMOR) => ironArmorG
      case t if (t == STEELSWORD) => steelSwordG
      case t if (t == WOODENSHIELD) => woodenShieldG
      case _ => missing
    }
  }
  
  /** returns equip texture of the given equipment */
  def imageEquipped(EquipmentType: Type): Texture = {
    EquipmentType match {
      case t if (t == KNIFE) => knifeE
      case t if (t == ROBES) => robesE
      case t if (t == IRONARMOR) => ironArmorE
      case t if (t == STEELSWORD) => steelSwordE
      case t if (t == WOODENSHIELD) => woodenShieldE
      case _ => missing
    }
  }

  /** returns armor of the given equipment */
  def armor(EquipmentType: Type): Double = {
    EquipmentType match {
      case t if (t == KNIFE) => 0
      case t if (t == ROBES) => 0
      case t if (t == IRONARMOR) => 1
      case t if (t == STEELSWORD) => 0
      case t if (t == WOODENSHIELD) => 0.5
      case _ => 0
    }
  }
  
  /** returns weight of the given equipment */
  def weight(EquipmentType: Type): Int = {
    EquipmentType match {
      case t if (t == KNIFE) => 2
      case t if (t == ROBES) => 0
      case t if (t == IRONARMOR) => 25
      case t if (t == STEELSWORD) => 12
      case t if (t == WOODENSHIELD) => 5
      case _ => 0
    }
  }
  
  /** returns block chance of the given equipment */
  def blockChance(EquipmentType: Type): Int = {
    EquipmentType match {
      case t if (t == KNIFE) => 0
      case t if (t == ROBES) => 0
      case t if (t == IRONARMOR) => 0
      case t if (t == STEELSWORD) => 0
      case t if (t == WOODENSHIELD) => 10
      case _ => 0
    }
  }
  
  /** Return damage of the given equipment.
   * 
   * Tuple3 includes number of dices, their number of eyes and additional flat bonus 
   * (num of dices, eyes, flat). Examples 2d3+5 = (2, 3, 5) and 1d4 = (1, 4, 0).
   */
  def damage(EquipmentType: Type): Tuple3[Int, Int, Int] = {
    EquipmentType match {
      case t if (t == KNIFE) => (1, 2, 0)
      case t if (t == ROBES) => (0, 0, 0)
      case t if (t == IRONARMOR) => (0, 0, 0)
      case t if (t == STEELSWORD) => (1, 6, 0)
      case t if (t == WOODENSHIELD) => (0, 0, 0)
      case _ => (0, 0, 0)
    }
  }
  
  /** returns armor piercing of the given equipment */
  def armorPiercing(EquipmentType: Type): Int = {
    EquipmentType match {
      case t if (t == KNIFE) => 0
      case t if (t == ROBES) => 0
      case t if (t == IRONARMOR) => 0
      case t if (t == STEELSWORD) => 1
      case t if (t == WOODENSHIELD) => 0
      case _ => 0
    }
  }
  
  /** returns accuracy of the given equipment */
  def accuracy(EquipmentType: Type): Int = {
    EquipmentType match {
      case t if (t == KNIFE) => 100
      case t if (t == ROBES) => 0
      case t if (t == IRONARMOR) => 0
      case t if (t == STEELSWORD) => 90
      case t if (t == WOODENSHIELD) => 0
      case _ => 0
    }
  }
  
  /** returns critical chance of the given equipment */
  def critChance(EquipmentType: Type): Int = {
    EquipmentType match {
      case t if (t == KNIFE) => 4
      case t if (t == ROBES) => 0
      case t if (t == IRONARMOR) => 0
      case t if (t == STEELSWORD) => 5
      case t if (t == WOODENSHIELD) => 0
      case _ => 0
    }
  }
  
  /** returns price of the given equipment */
  def price(EquipmentType: Type): Int = {
    EquipmentType match {
      case t if (t == KNIFE) => 20
      case t if (t == ROBES) => 10
      case t if (t == IRONARMOR) => 250
      case t if (t == STEELSWORD) => 400
      case t if (t == WOODENSHIELD) => 50
      case _ => 0
    }
  }
  
  /** returns name of the given equipment */
  def name(EquipmentType: Type): String = {
    EquipmentType match {
      case t if (t == KNIFE) => "Knife"
      case t if (t == ROBES) => "Robes"
      case t if (t == IRONARMOR) => "Iron armor"
      case t if (t == STEELSWORD) => "Steel sword"
      case t if (t == WOODENSHIELD) => "Wooden shield"
      case _ => "Unknown item name"
    }
  }
  
  /** returns description of the given equipment */
  def description(EquipmentType: Type): String = {
    EquipmentType match {
      case t if (t == KNIFE) => "TODO"
      case t if (t == ROBES) => "TODO"
      case t if (t == IRONARMOR) => "TODO"
      case t if (t == STEELSWORD) => "TODO"
      case t if (t == WOODENSHIELD) => "TODO"
      case _ => "Unknown item description"
    }
  }
  
}

/** Player usable consumables */
class Consumable(consumableName: String, consumableDescription: String, startX: Int, startY: Int, 
    consumableImage: String, consumablePrice: Int, isEquipped: Boolean) extends Item {
  
  val rnd = getRnd
  var name = consumableName
  var description = consumableDescription
  var x = startX * 32
  var y = startY * 32
  var image = loadTexture(consumableImage)
  var blockMovement = false
  var blockVision = false
  var isMonster = false
  var price = consumablePrice
  var inShop = false
  var equipped: Boolean = isEquipped
  
  init
  getConsumableList.append(this)
  
}

/** Player usable scrolls */
class Scroll(scrollName: String, scrollDescription: String, startX: Int, startY: Int, 
    scrollImage: String, scrollPrice: Int, isEquipped: Boolean) extends Item {
  
  val rnd = getRnd
  var name = scrollName
  var description = scrollDescription
  var x = startX * 32
  var y = startY * 32
  var image = loadTexture(scrollImage)
  var blockMovement = false
  var blockVision = false
  var isMonster = false
  var price = scrollPrice
  var inShop = false
  var equipped: Boolean = isEquipped
  
  init
  getScrollList.append(this)
  
}