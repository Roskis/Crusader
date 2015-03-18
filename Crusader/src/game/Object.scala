package game

import java.lang.Math.{abs, sqrt}

import scala.collection.mutable.Buffer

import org.newdawn.slick.opengl.Texture

import Direction._
import Main._
import Output.drawQuadTex
import Helpers._
import Prayers._

/** All of the game's objects will be under this trait */
trait Object {
  
  val rnd: scala.util.Random
  var name: String
  var description: String
  var x: Int
  var y: Int
  var blockMovement: Boolean
  var blockVision: Boolean
  
  /** Location to the object's texture */
  def image: Texture
  
  /** Add object to the tile it is on */
  def init() = if (getGrid.isWithinGrid(getX, getY)) getGrid.getTile(getX, getY).addObject(this)
  
  /** Draw the object to the screen */
  def draw = {
    if (image.getImageWidth == 32 && image.getImageHeight == 32)
      drawQuadTex(image, x - (getPlayer.getX - 16) * 32, 
          y - (getPlayer.getY - 8) * 32, image.getImageWidth, image.getImageHeight)
    else drawQuadTex(image, x - (getPlayer.getX - 16) * 32 - 16, y - (getPlayer.getY - 8) * 32 - 32, image.getImageWidth, image.getImageHeight)
  }
  
  /** Changes the position of this object
   *
   * @param newX x-coordinate
   * @param newY y-coordinate
   */
  def changePosition(newX: Int, newY: Int) = {
    if (getGrid.isWithinGrid(getX, getY)) getGrid.getTile(getX, getY).removeObject(this)
    x = newX * 32
    y = newY * 32
    if (getGrid.isWithinGrid(getX, getY)) getGrid.getTile(getX, getY).addObject(this)
  }

  /** get object's coordinate */
  def getCoordinate() = new Coordinate(getX, getY)
  
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

trait Character extends Object with Serializable {
  
  var health: Double
  var experience: Double
  var gold: Double
  var piety: Double
  var effectList: Buffer[Effect]
  def armor: Double
  def accuracy: Int
  def ap: Double
  def crit: Int
  
}

/** User's controllable player character */
class Player(playerName: String, startX: Int, startY: Int) extends Character with Serializable {
  
  val rnd = getRnd
  var name = playerName
  var description = "TODO"
  var x = startX * 32
  var y = startY * 32
  def image = playerImage
  var blockMovement = true
  var blockVision = false
  
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
  
  var slotWeapon: Equipment = new Equipment(-100, -100, ItemType.KNIFE, true)
  var slotArmor: Equipment = new Equipment(-100, -100, ItemType.ROBES, true)
  var slotShield: Equipment = null
  var slotRing: Equipment = null
  var slotAmulet: Equipment = null
  var slotUseable: Useable = null
  
  var effectList = Buffer[Effect]()
  
  /** When player succeeds praying one random prayer is selected */
  def pray = {
    if (rnd.nextInt(100) <= prayChance) {
      chooseRandomPrayer(getPrayerChances) match {
        case p if (p == PARTIALRESTORATION) => partialRestoration
        case p if (p == FULLRESTORATION) => fullRestoration
        case p if (p == LIGHTNINGBOLT) => lightningBolt
        case p if (p == GOLDLOSS) => goldLoss
        case p if (p == EXPERIENCELOSS) => experienceLoss
        case p if (p == DEMENTIA) => dementia
        case p if (p == STAIRS) => stairs
        case p if (p == SMITE) => smite
        case p if (p == GOLDGAIN) => goldGain
        case p if (p == EXPERIENCEGAIN) => experienceGain
        case p if (p == CLAIRVOYANCE) => clairvoyance
        case _ => {println("prayer not found")}
      }
    }
    else addLog("You pray.")
    if (getPlayer.piety > 0) getPlayer.piety -= (getPlayer.piety*0.05 + 5)
    else getPlayer.piety -= rnd.nextInt(5)+6
  }
  
  /** modifier applied to prays */
  def prayChance = 10 + (charity*2.5) + 
  (if (getX == getGrid.getAltar.getX && getY == getGrid.getAltar.getY) 20 else 0)
  
  /** modifier applied to all expirience gained */
  def giveXP(amount: Double) = experience += amount * (1+0.1*diligence)
  
  /** modifier applied to all gold gained */
  def giveGold(amount: Double) = gold += amount * (1+0.1*diligence)
  
  /** modifier applied to all piety gained */
  def givePiety(amount: Double) = piety += amount * (1+0.1*charity)
  
  /** Player's maximum health */
  def maxHealth: Int = 20+8*kindness
  
  /** Title of player */
  def title: String = {
    if (piety < -2000) "the Evil"
    else if (piety < -1000) "the Baleful"
    else if (piety < -500) "the Wicked"
    else if (piety < -200) "the Malignant"
    else if (piety < -100) "the Cursed"
    else if (piety < 0) "the Defiled"
    else if (piety > 25000) "the Avatar of God"
    else if (piety > 10000) "the Godly"
    else if (piety > 5000) "the Messiah"
    else if (piety > 2000) "the Saint"
    else if (piety > 1000) "the Sacred Crusader"
    else if (piety > 500) "the Holy"
    else if (piety > 200) "the Pure"
    else if (piety > 100) "the Paladin"
    else "the Knight"
  }
  
  /** Total level of player */
  def totalLevel: Int = zeal + humility + temperance + kindness + patience + charity + diligence
  
  /** Return smallest level xp requirement */
  def smallestLevel(): Int = {
    var skills = List(zeal, humility, temperance, kindness, patience, charity, diligence)
    skills = skills.sortWith(_ < _)
    xpNeededForLevel(skills(0))
  }
  
  /** Method to deal damage to monsters */
  def attack(target: Monster) {
    if (rnd.nextInt(100) <= accuracy - target.dodge) {
      target.takeDamage(damage(rnd.nextInt(100) <= crit), ap, this)
    }
    else target.takeDamage(smallestDamage, ap, this)
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
  
  def takeDamage(damage: Int, armorPierce: Double, attacker: Object) = {
    var effectiveArmor = armor
    if (rnd.nextInt(100) <= blockChance) effectiveArmor += shieldArmor
    if (effectiveArmor < 0) effectiveArmor = 0
    var effectiveDamage = (damage - effectiveArmor)
    if (effectiveDamage < 0) effectiveDamage = 0
    health -= effectiveDamage
    addLog(attacker.name + " deals " + effectiveDamage.toInt.toString + " damage to " + name + ".")
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
  
  def ap: Double = if (slotWeapon != null) slotWeapon.armorPiercing else 0
  
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
    if (slotUseable != null) num += slotUseable.weight
    ((1 - (0.05*patience)).toInt * num)
  }
  
  /** Return dodge of player */
  def dodge: Int = 100 - totalWeight + humility*2
  
  /** Return accuracy of player */
  def accuracy: Int = if (slotWeapon != null) slotWeapon.accuracy + temperance*2 else 100 + temperance*2
  
  /** Move to given coordinates or go to next map */
  def move(coord: Coordinate) = changePosition(coord.getX, coord.getY)
  
  /** Wait one turn, pick up item or equip item */
  def waitTurn {
    for (obj <- getGrid.getTile(getX, getY).getObjectList - this) {
      obj match {
        case item: Item => {
          if(item.inShop && getPlayer.gold >= item.price) {
            addLog("SOLD!!!")
            item.buy
          }
          else if (item.inShop) {
            addLog("You don't have enought gold.")
          }
          else item.pickUp
        }
        case _ => {}
      }
    }
  }
  
  /** Move and attack command */
  def moveOrAttack(direction: Direction.Value) = {
    val coord = getCoordinates(direction, getX, getY)
    if (getGrid.isWithinGrid(coord.getX, coord.getY)) {
      var canMove: Boolean = true
      for (obj <- getGrid.getTile(coord.getX, coord.getY).getObjectList) {
        obj match {
          case monster: Monster => {
            canMove = false
            attack(monster)
          }
          case item: Item => {
            if (item.inShop) addLog(item.name + " is " + item.price + " gold.")
            else if (ItemType.slot(item.itemType) == "item" && slotUseable == null) item.pickUp
          }
          case _ => {}
        }
      }
      if (canMove && !getGrid.getTile(coord.getX, coord.getY).blockMovement) move(coord)
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
    }
    else drawQuadTex(playerGrave, 16 * 32, 8 * 32, playerGrave.getImageWidth, playerGrave.getImageHeight)
  }
  
}

/** Passive objects are mostly decorative, but might also block some movement */
class PassiveObject(objectName: String, objectDescription: String, startX: Int, startY: Int, 
    objectImage: String) extends Object with Serializable {
  
  val rnd = getRnd
  var name = objectName
  var description = objectDescription
  var x = startX * 32
  var y = startY * 32
  def image = loadTexture(objectImage)
  var blockMovement = false
  var blockVision = false
  
  init
  getPassiveObjectList.append(this)
  
}

/** All of monsters and npc will be under this class */
class Monster(startX: Int, startY: Int, monsterType: MonsterType.Value) extends Character with Serializable {
  
  val mType = monsterType
  val rnd = getRnd
  var name = MonsterType.name(mType)
  var description = MonsterType.description(mType)
  var x = startX * 32
  var y = startY * 32
  def image = MonsterType.image(mType)
  var blockMovement = true
  var blockVision = false
  var mode = "passive"
  
  var health: Double = MonsterType.maxHP(mType)
  def armor: Double = MonsterType.armor(mType)
  def damage = MonsterType.damage(mType)
  def accuracy = MonsterType.accuracy(mType)
  def ap: Double = MonsterType.armorPierce(mType)
  def crit = MonsterType.criticalChance(mType)
  var experience = MonsterType.experience(mType).toDouble
  var gold = MonsterType.gold(mType).toDouble
  var piety = MonsterType.piety(mType).toDouble
  var dodge = MonsterType.dodge(mType)
  var blockChance = 0
  var shieldArmor = 0
  
  var effectList = Buffer[Effect]()
  
  init
  getMonsterList.append(this)
  
  /** When monster dies this method is called */
  def kill() {
    getMonsterList.filter(_ == this) foreach {getMonsterList -= _}
    updateLastMonster(null)
    getGrid.getTile(getX, getY).removeObject(this)
    if (this.monsterType == MonsterType.RAT) new Useable(getX, getY, ItemType.RATMEAT, false)
    x = -100
    y = -100
    addLog(name + " dies.")
    getPlayer.giveXP(experience)
    getPlayer.giveGold(gold)
    getPlayer.givePiety(piety)
  }
  
  /** Takes damage from attack */
  def takeDamage(damage: Int, armorPierce: Double, attacker: Object) = {
    updateLastMonster(this)
    var effectiveArmor = armor
    if (rnd.nextInt(100) <= blockChance) effectiveArmor += shieldArmor
    if (effectiveArmor < 0) effectiveArmor = 0
    var effectiveDamage = (damage - effectiveArmor)
    if (effectiveDamage < 0) effectiveDamage = 0
    health -= effectiveDamage
    addLog(attacker.name + " deals " + effectiveDamage.toInt.toString + " damage to " + name + ".")
    mode = "aggressive"
    if (health <= 0) kill
  }
  
  /** Method to deal damage to player */
  def attack(target: Player) {
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
  
  /** Monster's turn depends on it's ai */
  def turn() {
    for (effect <- effectList) effect.turn
    effectList = effectList.filter(_.duration <= 0)
    monsterType match {
      case m if (m == MonsterType.BAT) => batAI
      case m if (m == MonsterType.RAT) => ratAI
      case _ => basicAI
    }
  }
  
  /** AI for bat */
  def batAI = {
    if (mode == "passive") move(randomDirection(8))
    else if (mode == "aggressive") tryAttack
  }
  
  /** AI for rat */
  def ratAI = {
    if (mode == "passive" && distance(getPlayer) <= 3) mode = "flee"
    else if (mode == "flee") move(getCoordinates(getDirection(getPlayer.getCoordinate, getCoordinate), this))
    else if (mode == "aggressive") tryAttack
  }
  
  /** Simple ai for most of the monsters */
  def basicAI = {
    if (distance(getPlayer) > 7 && mode == "passive") {}
    else {
      mode = "aggressive"
      tryAttack
    }
  }
  
  /** Either move towards player or attack player */
  def tryAttack() = {
    if (distance(getPlayer) < 2) attack(getPlayer)
    else move(getGrid.line(getX, getY, getPlayer.getX, getPlayer.getY)(1))
  }
  
  /** Move monster to given coordinate */
  def move(coord: Coordinate): Unit = {
    if (getGrid.isWithinGrid(coord.getX, coord.getY)) {
      var boo = true
      for (obj <- getGrid.getTile(coord.getX, coord.getY).getObjectList) if (obj.isInstanceOf[Monster]) boo = false
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

object MonsterType extends Enumeration with Serializable {

  type Monster = Value
  val RAT, BAT, SNAKE, SPIDER, GOBLINA, GOBLINB, HOUND, LIZARDA, LIZARDB, LIZARDC, CROCODILE = Value
  
  /** return chances how monsters occur in game */
  def levelChance(level: Int): Map[Monster, Int] = {
    var chances = Map[Monster, Int]()
    level match {
      case l if (l == 1) => chances = 
        Map(RAT -> 10, BAT -> 50, SNAKE -> 25, SPIDER -> 25, GOBLINA -> 5, GOBLINB -> 1, 
            HOUND -> 2)
      case l if (l == 2) => chances = 
        Map(RAT -> 10, BAT -> 25, SNAKE -> 25, SPIDER -> 25, GOBLINA -> 16, GOBLINB -> 4, 
            HOUND -> 10)
      case l if (l == 3) => chances = 
        Map(RAT -> 10, BAT -> 15, SNAKE -> 10, SPIDER -> 10, GOBLINA -> 20, GOBLINB -> 5, 
            HOUND -> 15, LIZARDA -> 10, LIZARDB -> 4, LIZARDC -> 2, CROCODILE -> 1)
      case l if (l == 4) => chances = 
        Map(RAT -> 10, GOBLINA -> 8, GOBLINB -> 2, HOUND -> 5, LIZARDA -> 20, LIZARDB -> 8, 
            LIZARDC -> 2, CROCODILE -> 5)
      case _ => {chances = Map(BAT -> 100)}
    }
    chances
  }
  
  /** returns texture of the given monster */
  def image(MonsterType: Monster): Texture = {
    MonsterType match {
      case t if (t == RAT) => rat
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
  def maxHP(MonsterType: Monster): Int = {
    MonsterType match {
      case t if (t == RAT) => 2
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
  def damage(MonsterType: Monster): Tuple3[Int, Int, Int] = {
    MonsterType match {
      case t if (t == RAT) => (1, 1, 1)
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
  def armor(MonsterType: Monster): Double = {
    MonsterType match {
      case t if (t == RAT) => 0
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
  def accuracy(MonsterType: Monster): Int = {
    MonsterType match {
      case t if (t == RAT) => 100
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
  def criticalChance(MonsterType: Monster): Int = {
    MonsterType match {
      case t if (t == RAT) => 0
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
  def dodge(MonsterType: Monster): Int = {
    MonsterType match {
      case t if (t == RAT) => 20
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
  def armorPierce(MonsterType: Monster): Int = {
    MonsterType match {
      case t if (t == RAT) => 0
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
  def gold(MonsterType: Monster): Int = {
    MonsterType match {
      case t if (t == RAT) => 0
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
  def experience(MonsterType: Monster): Int = {
    MonsterType match {
      case t if (t == RAT) => 1
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
  def piety(MonsterType: Monster): Int = {
    MonsterType match {
      case t if (t == RAT) => 2
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
  def name(MonsterType: Monster): String = {
    MonsterType match {
      case t if (t == RAT) => "Rat"
      case t if (t == BAT) => "Bat"
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
  def description(MonsterType: Monster): String = {
    MonsterType match {
      case t if (t == RAT) => "TODO"
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
trait Item extends Object with Serializable {
  
  var price: Int
  var inShop: Boolean
  var equipped: Boolean
  val itemType: ItemType.Value
  val weight: Int
  
  def buy = {
    getPlayer.gold -= price
    inShop = false
    pickUp
  }
  
  def imageEquipped: Texture
  def unequip: Unit
  def use: Unit
  
  /** equip item */
  def equip {
    this match {
      case u: Useable => {
        getPlayer.slotUseable = u
        getUseableList.filter(_ == u) foreach {getUseableList -= _}
      }
      case e: Equipment => {
        if (ItemType.slot(itemType) == "weapon") getPlayer.slotWeapon = e
        else if (ItemType.slot(itemType) == "armor") getPlayer.slotArmor = e
        else if (ItemType.slot(itemType) == "shield") getPlayer.slotShield = e
        else if (ItemType.slot(itemType) == "ring") getPlayer.slotRing = e
        else if (ItemType.slot(itemType) == "amulet") getPlayer.slotAmulet = e
        getEquipmentList.filter(_ == e) foreach {getEquipmentList -= _}
      }
      case _ => {}
    }
    getGrid.getTile(getX, getY).removeObject(this)
    equipped = true
    x = -100
    y = -100
  }
  
  /** Pick up */
  def pickUp = {
    ItemType.slot(itemType) match {
      case s if (s == "weapon") => if (getPlayer.slotWeapon != null) getPlayer.slotWeapon.unequip
      case s if (s == "armor") => if (getPlayer.slotArmor != null) getPlayer.slotArmor.unequip
      case s if (s == "shield") => if (getPlayer.slotShield != null) getPlayer.slotShield.unequip
      case s if (s == "ring") => if (getPlayer.slotRing != null) getPlayer.slotRing.unequip
      case s if (s == "amulet") => if (getPlayer.slotAmulet != null) getPlayer.slotAmulet.unequip
      case s if (s == "item") => if (getPlayer.slotUseable != null) getPlayer.slotUseable.unequip
      case _ =>
    }
    equip
  }
  
}

/** Player usable equipments */
class Equipment(startX: Int, startY: Int, equipmentType: ItemType.Value, isEquipped: Boolean) extends Item with Serializable {
  
  val rnd = getRnd
  val itemType = equipmentType
  var name = ItemType.name(itemType)
  var description = ItemType.description(itemType)
  var x = startX * 32
  var y = startY * 32
  def image = ItemType.imageGround(itemType)
  def imageEquipped = ItemType.imageEquipped(itemType)
  var blockMovement = false
  var blockVision = false
  var price = ItemType.price(itemType)
  var inShop = false
  val armor = ItemType.armor(itemType)
  val weight = ItemType.weight(itemType)
  val blockChance = ItemType.blockChance(itemType)
  val damage = ItemType.damage(itemType)
  val armorPiercing = ItemType.armorPiercing(itemType)
  val accuracy = ItemType.accuracy(itemType)
  val critChance = ItemType.critChance(itemType)
  var equipped: Boolean = isEquipped
  
  if (!equipped) {
    init
    getEquipmentList.append(this)
  }
  
  /** dummy method */
  def use = {}
  
  /** unequip item */
  def unequip {
    if (getPlayer.slotWeapon != null && ItemType.slot(getPlayer.slotWeapon.itemType) == ItemType.slot(itemType)) getPlayer.slotWeapon = null
    else if (getPlayer.slotArmor != null && ItemType.slot(getPlayer.slotArmor.itemType) == ItemType.slot(itemType)) getPlayer.slotArmor = null
    else if (getPlayer.slotShield != null && ItemType.slot(getPlayer.slotShield.itemType) == ItemType.slot(itemType)) getPlayer.slotShield = null
    else if (getPlayer.slotRing != null && ItemType.slot(getPlayer.slotRing.itemType) == ItemType.slot(itemType)) getPlayer.slotRing = null
    else if (getPlayer.slotAmulet != null && ItemType.slot(getPlayer.slotAmulet.itemType) == ItemType.slot(itemType)) getPlayer.slotAmulet = null
    equipped = false
    x = getPlayer.getX * 32
    y = getPlayer.getY * 32
    init
    getEquipmentList.append(this)
  }
}

object ItemType extends Enumeration with Serializable {

  type Item = Value
  val KNIFE, ROBES, IRONARMOR, STEELSWORD, WOODENSHIELD, RATMEAT, SMALLHEALPOTION = Value
  
  /** return chances how items occur in game */
  def levelChance(level: Int): Map[Item, Int] = {
    var chances = Map[Item, Int]()
    level match {
      case l if (l == 1) => chances = 
        Map(KNIFE -> 1, ROBES -> 1, WOODENSHIELD -> 2, STEELSWORD -> 1, IRONARMOR -> 1, SMALLHEALPOTION -> 2)
      case l if (l == 2) => chances = 
        Map(KNIFE -> 1, ROBES -> 1, WOODENSHIELD -> 2, STEELSWORD -> 1, IRONARMOR -> 1, SMALLHEALPOTION -> 2)
      case l if (l == 3) => chances = 
        Map(WOODENSHIELD -> 2, STEELSWORD -> 2, IRONARMOR -> 2, SMALLHEALPOTION -> 2)
      case l if (l == 4) => chances = 
        Map(WOODENSHIELD -> 1, STEELSWORD -> 2, IRONARMOR -> 2, SMALLHEALPOTION -> 2)
      case _ => {chances = Map(KNIFE -> 100)}
    }
    chances
  }
  
  /** returns slot of the given equipment */
  def slot(ItemType: Item): String = {
    ItemType match {
      case t if (t == KNIFE) => "weapon"
      case t if (t == ROBES) => "armor"
      case t if (t == IRONARMOR) => "armor"
      case t if (t == STEELSWORD) => "weapon"
      case t if (t == WOODENSHIELD) => "shield"
      case t if (t == RATMEAT) => "item"
      case t if (t == SMALLHEALPOTION) => "item"
      case _ => ""
    }
  }
  
  /** returns ground texture of the given equipment */
  def imageGround(ItemType: Item): Texture = {
    ItemType match {
      case t if (t == KNIFE) => knifeG
      case t if (t == ROBES) => robesG
      case t if (t == IRONARMOR) => ironArmorG
      case t if (t == STEELSWORD) => steelSwordG
      case t if (t == WOODENSHIELD) => woodenShieldG
      case t if (t == RATMEAT) => ratG
      case t if (t == SMALLHEALPOTION) => smallhealpotion
      case _ => missing
    }
  }
  
  /** returns equip texture of the given equipment */
  def imageEquipped(ItemType: Item): Texture = {
    ItemType match {
      case t if (t == KNIFE) => knifeE
      case t if (t == ROBES) => robesE
      case t if (t == IRONARMOR) => ironArmorE
      case t if (t == STEELSWORD) => steelSwordE
      case t if (t == WOODENSHIELD) => woodenShieldE
      case t if (t == RATMEAT) => ratE
      case t if (t == SMALLHEALPOTION) => smallhealpotion
      case _ => missing
    }
  }

  /** returns armor of the given equipment */
  def armor(ItemType: Item): Double = {
    ItemType match {
      case t if (t == KNIFE) => 0
      case t if (t == ROBES) => 0
      case t if (t == IRONARMOR) => 1
      case t if (t == STEELSWORD) => 0
      case t if (t == WOODENSHIELD) => 0.5
      case _ => 0
    }
  }
  
  /** returns weight of the given equipment */
  def weight(ItemType: Item): Int = {
    ItemType match {
      case t if (t == KNIFE) => 2
      case t if (t == ROBES) => 0
      case t if (t == IRONARMOR) => 25
      case t if (t == STEELSWORD) => 12
      case t if (t == WOODENSHIELD) => 5
      case t if (t == RATMEAT) => 3
      case t if (t == SMALLHEALPOTION) => 2
      case _ => 0
    }
  }
  
  /** returns block chance of the given equipment */
  def blockChance(ItemType: Item): Int = {
    ItemType match {
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
  def damage(ItemType: Item): Tuple3[Int, Int, Int] = {
    ItemType match {
      case t if (t == KNIFE) => (1, 2, 0)
      case t if (t == ROBES) => (0, 0, 0)
      case t if (t == IRONARMOR) => (0, 0, 0)
      case t if (t == STEELSWORD) => (1, 6, 0)
      case t if (t == WOODENSHIELD) => (0, 0, 0)
      case _ => (0, 0, 0)
    }
  }
  
  /** returns armor piercing of the given equipment */
  def armorPiercing(ItemType: Item): Int = {
    ItemType match {
      case t if (t == KNIFE) => 0
      case t if (t == ROBES) => 0
      case t if (t == IRONARMOR) => 0
      case t if (t == STEELSWORD) => 1
      case t if (t == WOODENSHIELD) => 0
      case _ => 0
    }
  }
  
  /** returns accuracy of the given equipment */
  def accuracy(ItemType: Item): Int = {
    ItemType match {
      case t if (t == KNIFE) => 100
      case t if (t == ROBES) => 0
      case t if (t == IRONARMOR) => 0
      case t if (t == STEELSWORD) => 90
      case t if (t == WOODENSHIELD) => 0
      case _ => 0
    }
  }
  
  /** returns critical chance of the given equipment */
  def critChance(ItemType: Item): Int = {
    ItemType match {
      case t if (t == KNIFE) => 4
      case t if (t == ROBES) => 0
      case t if (t == IRONARMOR) => 0
      case t if (t == STEELSWORD) => 5
      case t if (t == WOODENSHIELD) => 0
      case _ => 0
    }
  }
  
  /** returns price of the given equipment */
  def price(ItemType: Item): Int = {
    ItemType match {
      case t if (t == KNIFE) => 20
      case t if (t == ROBES) => 10
      case t if (t == IRONARMOR) => 250
      case t if (t == STEELSWORD) => 400
      case t if (t == WOODENSHIELD) => 50
      case t if (t == RATMEAT) => 25
      case t if (t == SMALLHEALPOTION) => 50
      case _ => 0
    }
  }
  
  /** returns name of the given equipment */
  def name(ItemType: Item): String = {
    ItemType match {
      case t if (t == KNIFE) => "Knife"
      case t if (t == ROBES) => "Robes"
      case t if (t == IRONARMOR) => "Iron armor"
      case t if (t == STEELSWORD) => "Steel sword"
      case t if (t == WOODENSHIELD) => "Wooden shield"
      case t if (t == RATMEAT) => "Rat meat"
      case t if (t == SMALLHEALPOTION) => "Small healing salve"
      case _ => "Unknown item name"
    }
  }
  
  /** returns description of the given equipment */
  def description(ItemType: Item): String = {
    ItemType match {
      case t if (t == KNIFE) => "TODO"
      case t if (t == ROBES) => "TODO"
      case t if (t == IRONARMOR) => "TODO"
      case t if (t == STEELSWORD) => "TODO"
      case t if (t == WOODENSHIELD) => "TODO"
      case t if (t == RATMEAT) => "TODO"
      case t if (t == SMALLHEALPOTION) => "TODO"
      case _ => "Unknown item description"
    }
  }
  
}

/** Player usable consumables */
class Useable(startX: Int, startY: Int, val itemType: ItemType.Value, isEquipped: Boolean) extends Item with Serializable {

  val rnd = getRnd
  val weight = ItemType.weight(itemType)
  var name = ItemType.name(itemType)
  var description = ItemType.description(itemType)
  var x = startX * 32
  var y = startY * 32
  def image = ItemType.imageGround(itemType)
  def imageEquipped = ItemType.imageEquipped(itemType)
  var blockMovement = false
  var blockVision = false
  var price = ItemType.price(itemType)
  var inShop = false
  var equipped = isEquipped
  
  if (!equipped) {
    init
    getUseableList.append(this)
  }

  
  /** Unequip item */
  def unequip {
    if (getPlayer.slotUseable != null) getPlayer.slotUseable = null
    equipped = false
    x = getPlayer.getX * 32
    y = getPlayer.getY * 32
    init
    getUseableList.append(this)
  }

  /** Use item */
  def use = {
    itemType match {
      case i if (i == ItemType.RATMEAT) => {
        getPlayer.health += roll(6)
        if (getPlayer.health > getPlayer.maxHealth) getPlayer.health = getPlayer.maxHealth
        addLog(getPlayer.name + " eats " + name + ".")

      }
      case i if (i == ItemType.SMALLHEALPOTION) => {
        getPlayer.effectList = getPlayer.effectList :+ new smallHeal(roll(5)+5, getPlayer)
        addLog(getPlayer.name + " drinks " + name + ".")
      }
      case _ => {}
    }
    getPlayer.slotUseable = null
    equipped = false
    x = -100
    y = -100
  }
}