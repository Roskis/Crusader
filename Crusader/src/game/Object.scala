package game

import java.lang.Math.{abs, sqrt}

import scala.collection.mutable.Buffer

import org.newdawn.slick.opengl.Texture
import org.newdawn.slick.Color

import scala.xml.XML

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
    else drawQuadTex(image, x - (getPlayer.getX - 16) * 32 - 16, y - (getPlayer.getY - 8) * 32 - 32, 
        image.getImageWidth, image.getImageHeight)
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

  /** Return information about this object */
  def infoBox: Buffer[Buffer[(String, Color)]]
  
  /** Get object's coordinate */
  def getCoordinate() = new Coordinate(getX, getY)
  
  /** Get object's tile */
  def getTile() = getGrid.getTile(getX, getY)
  
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
  def accuracy: Double
  def ap: Double
  def crit: Double
  def move(coord: Coordinate): Unit
  
}

/** User's controllable player character */
class Player(playerName: String, startX: Int, startY: Int) extends Character with Serializable {
  
  val rnd = getRnd
  var name = playerName
  var description = "-"
  var x = startX * 32
  var y = startY * 32
  def image = playerImage
  var blockMovement = true
  var blockVision = false
  
  var health: Double = 20
  var experience: Double = 0
  var gold: Double = 0
  var piety: Double = 0
  
  var zeal: Double = 0
  var humility: Double = 0
  var temperance: Double = 0
  var kindness: Double = 0
  var patience: Double = 0
  var charity: Double = 0
  var diligence: Double = 0
  
  var slotWeapon: Equipment = new Equipment(-100, -100, ItemType.KNIFE, true)
  var slotArmor: Equipment = new Equipment(-100, -100, ItemType.ROBES, true)
  var slotShield: Equipment = null
  var slotRing: Equipment = null
  var slotAmulet: Equipment = null
  var slotUseable: Useable = null
  
  var effectList = Buffer[Effect]()
  def itemList = {
    var list = Buffer[Equipment]()
    if (slotWeapon != null) list.append(slotWeapon)
    if (slotArmor != null) list.append(slotArmor)
    if (slotShield != null) list.append(slotShield)
    if (slotRing != null) list.append(slotRing)
    if (slotAmulet != null) list.append(slotAmulet)
    list
  }
  
  def infoBox: Buffer[Buffer[(String, Color)]] = {
    Buffer[Buffer[(String, Color)]](Buffer(("Name: ", Color.black), (name, Color.black)))
  }
  
  def getZeal: Double = {
    var num: Double = zeal
    for (effect <- effectList) num += EffectType.zeal(effect.effectType)
    for (item <- itemList) num += ItemType.zeal(item.itemType)
    num
  }
  
  def getHumility: Double = {
    var num: Double = humility
    for (effect <- effectList) num += EffectType.humility(effect.effectType)
    for (item <- itemList) num += ItemType.humility(item.itemType)
    num
  }
  
  def getTemperance: Double = {
    var num: Double = temperance
    for (effect <- effectList) num += EffectType.temperance(effect.effectType)
    for (item <- itemList) num += ItemType.temperance(item.itemType)
    num
  }
  
  def getKindness: Double = {
    var num: Double = kindness
    for (effect <- effectList) num += EffectType.kindness(effect.effectType)
    for (item <- itemList) num += ItemType.kindness(item.itemType)
    num
  }
  
  def getPatience: Double = {
    var num: Double = patience
    for (effect <- effectList) num += EffectType.patience(effect.effectType)
    for (item <- itemList) num += ItemType.patience(item.itemType)
    num
  }
  
  def getCharity: Double = {
    var num: Double = charity
    for (effect <- effectList) num += EffectType.charity(effect.effectType)
    for (item <- itemList) num += ItemType.charity(item.itemType)
    num
  }
  
  def getDiligence: Double = {
    var num: Double = diligence
    for (effect <- effectList) num += EffectType.diligence(effect.effectType)
    for (item <- itemList) num += ItemType.diligence(item.itemType)
    num
  }
    
  /** Amount of piety defines view range */
  def viewRadius: Double = {
    var radius = (piety/100)
    if (radius > 5) radius = 5
    else if (radius < - 2) radius = -2
    radius += 5
    for (effect <- effectList) radius += EffectType.viewRange(effect.effectType)
    for (item <- itemList) radius += ItemType.viewRange(item.itemType)
    if (radius < 2) radius = 2
    radius
  }
  
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
        case p if (p == ITEM) => item
        case p if (p == FEAR) => fear
        case p if (p == AOEDAMAGE) => aoeDamage
        case p if (p == BLINDINGLIGHT) => blindingLight
        case p if (p == REVEALSECRET) => revealSecret
        case p if (p == IMMUNITY) => immunity
        case p if (p == BUFF) => buff
        case p if (p == VISION) => vision
        case p if (p == LEVELUP) => levelUp
        case p if (p == LEVELDOWN) => levelDown
        case p if (p == TIMESTOP) => timeStop
        case p if (p == TEMPBOOST) => tempBoost
        case _ => {println("prayer not found")}
      }
    }
    else addLog("You pray.")
    if (getPlayer.piety > 0) getPlayer.piety -= (getPlayer.piety*0.05 + 5)
    else getPlayer.piety -= rnd.nextInt(5)+6
  }
  
  /** Modifier applied to prays */
  def prayChance: Double = {
    var num: Double = 20 + getCharity*2.5
    for (effect <- effectList) num += EffectType.prayChance(effect.effectType)
    for (item <- itemList) num += ItemType.prayChance(item.itemType)
    for (neighbor <- getGrid.neighbors(getTile, 8)) for (obj <- neighbor.objectList) if (obj.isInstanceOf[Monster]) num += 10
    if (getX == getGrid.getAltar.getX && getY == getGrid.getAltar.getY && getLevel%5 == 0) num += 40
    else if (getX == getGrid.getAltar.getX && getY == getGrid.getAltar.getY) num += 20
    else if (getLevel%5 == 0) num += 10
    num
  }
  
  /** Modifier applied to all expirience gained */
  def giveXP(amount: Double) = {
    var num: Double = 1
    for (effect <- effectList) num *= EffectType.xpModifier(effect.effectType)
    for (item <- itemList) num *= ItemType.xpModifier(item.itemType)
    num
    experience += amount * (1 + 0.1*getDiligence) * num
  }
  
  /** Modifier applied to all gold gained */
  def giveGold(amount: Double) = {
    var num: Double = 1
    for (effect <- effectList) num *= EffectType.goldModifier(effect.effectType)
    for (item <- itemList) num *= ItemType.goldModifier(item.itemType)
    num
    gold += amount * (1 + 0.1*getDiligence) * num
  }
  
  /** Modifier applied to all piety gained */
  def givePiety(amount: Double) = {
    var num: Double = 1
    for (effect <- effectList) num *= EffectType.pietyModifier(effect.effectType)
    for (item <- itemList) num *= ItemType.pietyModifier(item.itemType)
    num
    piety += amount * (1 + 0.1*getCharity) * num
  }
  
  /** Player's maximum health */
  def maxHealth: Double = {
    var num: Double = 0
    for (effect <- effectList) num += EffectType.health(effect.effectType)
    for (item <- itemList) num += ItemType.health(item.itemType)
    num
    20 + 8*getKindness + num
  }
  
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
  def totalLevel: Double = zeal + humility + temperance + kindness + patience + charity + diligence
  
  /** Return smallest level xp requirement */
  def smallestLevel(): Double = {
    var skills = List(zeal, humility, temperance, kindness, patience, charity, diligence)
    skills = skills.sortWith(_ < _)
    xpNeededForLevel(skills(0).toInt)
  }
  
  /** Method to deal damage to monsters */
  def attack(target: Monster) {
    if (rnd.nextInt(100) >= accuracy) addLog(name.toUpperCase.head + name.tail + " misses.")
    else if (rnd.nextInt(100) <= target.dodge) addLog(target.name.toUpperCase.head + target.name.tail + " dodges.")
    else target.takeDamage(damage(rnd.nextInt(100) <= crit), ap, this)
  }
  
  /** Damage is based on player's weapon */
  def damage(crit: Boolean): Double = {
    var num: Double = 0
    for (effect <- effectList) num += effect.damage
    if (slotWeapon != null) for (item <- itemList) num += item.damage
    else {
      num += roll(2)
      for (item <- itemList) num += item.damage
    }
    if (crit) {
      addLog(name.toUpperCase.head + name.tail + " manages to make critical strike.")
      num += (getZeal+2) + roll((getZeal+2).toInt)
    }
    else num += roll((getZeal+2).toInt)
    num
  }
  
  def takeDamage(damage: Double, armorPierce: Double, attacker: Object) = {
    var effectiveArmor: Double = armor
    if (rnd.nextInt(100) <= blockChance) effectiveArmor += shieldArmor
    effectiveArmor -= armorPierce
    if (effectiveArmor < 0) effectiveArmor = 0
    var effectiveDamage = (damage - effectiveArmor)
    if (effectiveDamage < 0) effectiveDamage = 0
    health -= effectiveDamage
    addLog(attacker.name.toUpperCase.head + attacker.name.tail + " deals " + 
        effectiveDamage.toInt.toString + " damage to " + name + ".")
  }
  
  def armor: Double = {
    var num: Double = {
      if (getPatience > 7) 2
      else if (getPatience > 5) 1.5
      else if (getPatience > 3) 1
      else if (getPatience > 1) 0.5
      else 0
    }
    for (effect <- effectList) num += EffectType.armor(effect.effectType)
    for (item <- itemList) num += ItemType.armor(item.itemType)
    num
  }
  
  /** Armor poercing of weapon used */
  def ap: Double = {
    var num: Double = 0
    for (effect <- effectList) num += EffectType.armorPiercing(effect.effectType)
    for (item <- itemList) num += ItemType.armorPiercing(item.itemType)
    num
  }
  
  /** Critical chance of weapon used */
  def crit: Double = {
    var num: Double = 2
    for (effect <- effectList) num += EffectType.critChance(effect.effectType)
    for (item <- itemList) num += ItemType.critChance(item.itemType)
    num
  }
  
  /** Block chance of shield used */
  def blockChance: Double = {
    var num: Double = 0
    for (effect <- effectList) num += EffectType.blockChance(effect.effectType)
    for (item <- itemList) num += ItemType.blockChance(item.itemType)
    num
  }
  
  /** Shield Armor */
  def shieldArmor: Double = {
    var num: Double = 0
    for (effect <- effectList) num += EffectType.shieldArmor(effect.effectType)
    for (item <- itemList) num += ItemType.shieldArmor(item.itemType)
    num
  }
  
  /** Total weight of items after patience bonus */
  def totalWeight: Double = {
    var num: Double = 0
    for (effect <- effectList) num += EffectType.weight(effect.effectType)
    for (item <- itemList) num += ItemType.weight(item.itemType)
    ((1 - (0.05*getPatience)).toInt * num)
  }
  
  /** Return dodge of player */
  def dodge: Double = {
    var num: Double = getHumility*2 - totalWeight
    for (effect <- effectList) num += EffectType.dodge(effect.effectType)
    for (item <- itemList) num += ItemType.dodge(item.itemType)
    num
  }
  
  /** Return accuracy of player */
  def accuracy: Double = {
    var num: Double = if (slotWeapon != null) 0 + getTemperance*2 else 100 + getTemperance*2
    for (effect <- effectList) num += EffectType.accuracy(effect.effectType)
    for (item <- itemList) num += ItemType.accuracy(item.itemType)
    num
  }
  
  /** Move to given coordinates or go to next map */
  def move(coord: Coordinate) = {
    var cannotMove: Effect = null
    for (effect <- effectList) if (effect.isInstanceOf[bind]) cannotMove = effect
    if (cannotMove != null) addLog(name.toUpperCase.head + name.tail + " is binded by " + 
        cannotMove.caster.name.toUpperCase.head + cannotMove.caster.name.tail + ".")
    else changePosition(coord.getX, coord.getY)
  }
  
  /** Wait one turn, pick up item or equip item */
  def waitTurn {
    for (obj <- getGrid.getTile(getX, getY).getObjectList - this) {
      obj match {
        case item: Item => {
          if(item.inShop && getPlayer.gold >= item.price && getGrid.getDjinn.mode == "passive") {
            addLog("SOLD!!!")
            item.buy
          }
          else if (item.inShop && getGrid.getDjinn.mode == "passive") {
            addLog("You don't have enought gold.")
          }
          else item.pickUp
        }
        case _ => {}
      }
    }
  }
  
  /** Alternative methods to moveOrAttack */
  def moveOrAttack(tile: Tile) {moveOrAttack(new Coordinate(tile.getX, tile.getY))}
  def moveOrAttack(direction: Direction.Value) {moveOrAttack(getCoordinates(direction, getX, getY))}
  
  /** Move and attack command */
  def moveOrAttack(coord: Coordinate) = {
    if (getGrid.isWithinGrid(coord.getX, coord.getY)) {
      var canMove: Boolean = true
      for (obj <- getGrid.getTile(coord.getX, coord.getY).getObjectList) {
        obj match {
          case monster: Monster => {
            canMove = false
            attack(monster)
          }
          case item: Item => {
            if (item.inShop && getGrid.getDjinn.mode == "passive") 
              addLog(item.name.toUpperCase.head + item.name.tail + " is " + item.price.toInt + " gold.")
            else if (ItemType.slot(item.itemType) == "item" && slotUseable == null) item.pickUp
          }
          case pasobj: PassiveObject if (pasobj.pType == PassiveType.STAIRS) => {goToNextLevel}
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
      for (slot <- List(slotWeapon, slotArmor, slotShield)) {
        if (slot != null && slot.imageEquipped.getImageHeight.toInt == 32 && slot.imageEquipped.getImageWidth.toInt == 32) 
          drawQuadTex(slot.imageEquipped, 16 * 32, 8 * 32, slot.imageEquipped.getImageWidth, slot.imageEquipped.getImageHeight)
        else if (slot != null && slot.imageEquipped.getImageHeight == 64 && slot.imageEquipped.getImageWidth == 64)
          drawQuadTex(slot.imageEquipped, 16 * 32 - 16, 7 * 32, slot.imageEquipped.getImageWidth, slot.imageEquipped.getImageHeight)
        }
    }
    else drawQuadTex(playerGrave, 16 * 32, 8 * 32, playerGrave.getImageWidth, playerGrave.getImageHeight)
  }
  
}

/** Passive objects are mostly decorative, but might also block some movement */
class PassiveObject(objectName: String, objectDescription: String, startX: Int, startY: Int, 
    passiveType: PassiveType.Value) extends Object with Serializable {
  
  val rnd = getRnd
  val pType = passiveType
  var name = objectName
  var description = objectDescription
  var x = startX * 32
  var y = startY * 32
  def image = PassiveType.image(passiveType)
  var blockMovement = false
  var blockVision = false
  
  init
  getPassiveObjectList.append(this)
  
  def infoBox: Buffer[Buffer[(String, Color)]] = {
    Buffer[Buffer[(String, Color)]](Buffer(("Name: ", Color.black), (name, Color.black)))
  }
  
}

/** TODO */
object PassiveType extends Enumeration with Serializable {

  type Passive = Value
  val ALTAR1, ALTAR2, ROCK1, ROCK2, BIGTREE1, TREE1, STAIRS = Value

  /** returns texture of the given monster */
  def image(PassiveType: Passive): Texture = {
    PassiveType match {
      case t if (t == ALTAR1) => altar1
      case t if (t == ALTAR2) => altar2
      case t if (t == ROCK1) => rock1
      case t if (t == ROCK2) => rock2
      case t if (t == BIGTREE1) => bigTree1
      case t if (t == TREE1) => tree1
      case t if (t == STAIRS) => portalEp1
      case _ => missing
    }
  }
}

/** All of monsters and npc will be under this class */
class Monster(startX: Int, startY: Int, monsterType: MonsterType.Value) extends Character with Serializable {
  
  val mType = monsterType
  val rnd = getRnd
  var name = MonsterType.name(mType)
  var description = MonsterType.description(mType)
  var x = startX * 32
  var y = startY * 32
  def image = MonsterType.image(mType, if (mode == "passive") true else false)
  var blockMovement = true
  var blockVision = false
  var mode = "passive"
  var health: Double = MonsterType.maxHP(mType)
  def armor: Double = MonsterType.armor(mType)
  def damage = MonsterType.damage(mType)
  def ap: Double = MonsterType.armorPierce(mType)
  def crit = MonsterType.criticalChance(mType)
  var experience = MonsterType.experience(mType).toDouble
  var gold = MonsterType.gold(mType).toDouble
  var piety = MonsterType.piety(mType).toDouble
  var dodge: Double = MonsterType.dodge(mType)
  var blockChance: Double = 0
  var shieldArmor: Double = 0
  var usedAbility = false
  var effectList = Buffer[Effect]()
  var spellcd = 1
  var spellchannel = 0
  var extraToDraw = List[Texture]()
  
  init
  getMonsterList.append(this)
  
  def infoBox: Buffer[Buffer[(String, Color)]] = {
    Buffer[Buffer[(String, Color)]](Buffer(("Name: ", Color.black), (name, Color.black)))
  }
  
  /** Accuracy of the monster */
  def accuracy: Double = {
    var acc = MonsterType.accuracy(mType)
    for (effect <- effectList) acc+= EffectType.accuracy(effect.effectType)
    acc
  }
  
  /** When monster dies this method is called */
  def kill() {
    getMonsterList.filter(_ == this) foreach {getMonsterList -= _}
    updateLastMonster(null)
    getGrid.getTile(getX, getY).removeObject(this)
    MonsterType.drop(monsterType, getX, getY)
    if (distance(getGrid.getAltar) <= 2) {
      getPlayer.giveXP(2 * experience)
      getPlayer.givePiety(2 * piety)
    }
    else {
      getPlayer.giveXP(experience)
      getPlayer.giveGold(gold)
      getPlayer.givePiety(piety)
    }
    x = -100
    y = -100
    addLog(name.toUpperCase.head + name.tail + " dies.")

    if (mType == MonsterType.SLOTH) addLog(getPlayer.name.toUpperCase.head + getPlayer.name.tail + 
        " killed the first boss! Unfortunately demo ends here. Your Score is " + 
        getPlayer.gold.toInt + ".")
  }
  
  /** Takes damage from attack */
  def takeDamage(damage: Double, armorPierce: Double, attacker: Object) = {
    updateLastMonster(this)
    var effectiveArmor: Double = armor
    for (effect <- effectList) effectiveArmor+= EffectType.armor(effect.effectType)
    if (rnd.nextInt(100) <= blockChance) effectiveArmor += shieldArmor
    effectiveArmor -= armorPierce
    if (effectiveArmor < 0) effectiveArmor = 0
    var effectiveDamage = (damage - effectiveArmor)
    if (effectiveDamage < 0) effectiveDamage = 0
    health -= effectiveDamage
    addLog(attacker.name.toUpperCase.head + attacker.name.tail + " deals " + 
        effectiveDamage.toInt.toString + " damage to " + name.toUpperCase.head + name.tail + ".")
    if (mType != MonsterType.DJINN) mode = "aggressive"
    if (health <= 0) kill
  }
  
  /** Method to deal damage to player */
  def attack(target: Player) {
    if (rnd.nextInt(100) >= accuracy) addLog(name.toUpperCase.head + name.tail + " misses.")
    else if (rnd.nextInt(100) <= target.dodge) addLog(target.name.toUpperCase.head + target.name.tail + " dodges.")
    else target.takeDamage(damageroll(rnd.nextInt(100) <= crit), ap, this)
  }
  
  def damageroll(crit: Boolean): Double = {
    var dmg: Double = 0
    for (effect <- effectList) dmg += effect.damage
    dmg += damage
    if (crit) {
      addLog(name.toUpperCase.head + name.tail + " manages to make critical strike.")
      dmg + dmg
    }
    else dmg
  }
  
  /** Monster's turn depends on it's ai */
  def turn() {
    var canMove = true
    for (effect <- effectList) {
      effect.turn
      if (effect.isInstanceOf[fear]) canMove = false
    }
    effectList.filter(_.duration <= 0) foreach {effectList -= _}
    if (canMove) {
      monsterType match {
        case m if (m == MonsterType.BAT) => batAI
        case m if (m == MonsterType.RAT) => ratAI
        case m if (m == MonsterType.SNAKEB) => snakeAI
        case m if (m == MonsterType.LIZARDC) => lizardMageAI
        case m if (m == MonsterType.SLOTH) => slothAI
        case m if (m == MonsterType.SPIDER) => spiderAI
        case m if (m == MonsterType.DJINN) => djinnAI
        case _ => basicAI
      }
    }
  }
  
  /** AI for bat */
  def batAI = {
    if (mode == "passive") move(randomDirection(8))
    else if (mode == "aggressive") tryAttack
  }
  
  /** AI for rat */
  def ratAI = {
    if (mode == "passive" && distance(getPlayer) <= 4) mode = "flee"
    else if (mode == "flee" && distance(getPlayer) < 7) {
      if (rnd.nextBoolean) move(getCoordinates(getDirection(getPlayer.getCoordinate, getCoordinate), this))
      else if (rnd.nextInt(5) == 0) addLog(name.toUpperCase.head + name.tail + " squeaks.")
    }
    else if (mode == "aggressive") tryAttack
    else mode = "passive"
  }
  
  /** AI for spider */
  def spiderAI = {
    if (distance(getPlayer) > 6 && mode == "aggressive") {mode = "passive"}
    else if (distance(getPlayer) > 6 && mode == "passive") {}
    else if (distance(getPlayer) < 2 && rnd.nextInt(6) == 0) {
      addLog(name.toUpperCase.head + name.tail + " binds " + getPlayer.name + " with its sticky web.")
      getPlayer.effectList = getPlayer.effectList :+ new bind(roll(1)+1, getPlayer, this)
    }
    else {
      mode = "aggressive"
      tryAttack
    }
  }
  
  /** AI for snake */
  def snakeAI = {
    if (distance(getPlayer) > 7 && mode == "aggressive") {mode = "passive"}
    else if (distance(getPlayer) > 3 && mode == "passive") {
      if (distance(getPlayer) < 5 && rnd.nextInt(5) == 0) 
        addLog(name.toUpperCase.head + name.tail + " hisses.")
    }
    else if (distance(getPlayer) < 2 && !usedAbility) {
      usedAbility = true
      addLog(name.toUpperCase.head + name.tail + " bites " + getPlayer.name + " with its poisonous fangs.")
      getPlayer.effectList = getPlayer.effectList :+ new poison(roll(8), getPlayer, this)
    }
    else {
      mode = "aggressive"
      tryAttack
    }
  }
  
  /** AI for lizard mages */
  def lizardMageAI = {
    if (distance(getPlayer) > 7 && mode == "aggressive") {mode = "passive"}
    else if (distance(getPlayer) > 7 && mode == "passive") {}
    else if (getGrid.getTile(getX, getY).visible && distance(getPlayer) <= 4 && spellcd <= 0) {
      if (spellchannel <= 0) {
        spellcd = 5
        val dmg = roll(6)
        addLog(name.toUpperCase.head + name.tail + " Casts fireball dealing " + dmg + 
            " damage to " + getPlayer.name + ".")
        getPlayer.health -= dmg
        extraToDraw = List()
        }
      else {
      spellchannel -= 1
      if (spellchannel == 4) extraToDraw = List(fireballch5)
      else if (spellchannel == 3) extraToDraw = List(fireballch4)
      else if (spellchannel == 2) extraToDraw = List(fireballch3)
      else if (spellchannel == 1) extraToDraw = List(fireballch2)
      else if (spellchannel == 0) extraToDraw = List(fireballch1)
      }
    }
    else {
      if (mode == "passive") {
        mode == "aggressive"
        spellcd = 1
      }
      if (distance(getPlayer) > 4) move(getGrid.line(getX, getY, getPlayer.getX, getPlayer.getY)(1))
      else if (distance(getPlayer) < 4) move(getCoordinates(getDirection(getPlayer.getCoordinate, getCoordinate), this))
      extraToDraw = List()
      spellchannel = 5
      spellcd -= 1
    }
  }
  
  /** AI for sloth demon */
  def slothAI = {
    if (distance(getPlayer) > 6 && mode == "aggressive") {mode = "passive"}
    else if (distance(getPlayer) > 6 && mode == "passive") {
      if (rnd.nextInt(5) == 0) health += 1
      if (health > MonsterType.maxHP(mType)) health = MonsterType.maxHP(mType)
    }
    else if (rnd.nextInt(4) != 0) {
      mode = "aggressive"
      tryAttack
    }
  }
  
  /** AI for djinn's */
  def djinnAI = {
    if ((health == MonsterType.maxHP(mType) || health == spellchannel) && mode == "passive") {}
    else if (!usedAbility) {
      addLog("I warn you mortal! Do not try to fight me.")
      usedAbility = true
      spellchannel = health.toInt
      }
    else {
      mode = "aggressive"
      tryAttack
    }
  }
  
  /** Simple ai for most of the monsters */
  def basicAI = {
    if (distance(getPlayer) > 7 && mode == "aggressive") {mode = "passive"}
    else if (distance(getPlayer) > 7 && mode == "passive") {}
    else {
      mode = "aggressive"
      tryAttack
    }
  }
  
  /** Either move towards player or attack player */
  def tryAttack() = {
    if (distance(getPlayer) < 2) attack(getPlayer)
    else {
      val goto = getDirection(new Coordinate(this.getX, this.getY), new Coordinate(getPlayer.getX, getPlayer.getY))
      if (getGrid.getTile(getCoordinates(goto, getX, getY)).blockMovement) {
        move(getGrid.dijkstra(getTile, getGrid.getTile(getPlayer.getX, getPlayer.getY)).drop(1)(0).getCoordinate)
      }
      else move(goto)
      }
  }

  /** Move monster to given coordinate */
  def move(coord: Coordinate): Unit = {
    var cannotMove: Effect = null
    for (effect <- effectList) if (effect.isInstanceOf[bind]) cannotMove = effect
    if (getGrid.isWithinGrid(coord.getX, coord.getY)) {
      var boo = true
      for (obj <- getGrid.getTile(coord.getX, coord.getY).getObjectList) 
        if (obj.isInstanceOf[Monster]) boo = false
      if (cannotMove != null) addLog(name.toUpperCase.head + name.tail + " is binded by " + 
        cannotMove.caster.name.toUpperCase.head + cannotMove.caster.name.tail + ".")
      else if (distance(coord) < 2 && !getGrid.getTile(coord.getX, coord.getY).blockMovement && boo && 
          getGrid.getTile(coord.getX, coord.getY).tileType != TileType.SECRETDOOR) {
        changePosition(coord.getX, coord.getY)
      }
    }
  }
  
  /** Move the object to given direction */
  def move(direction: Direction.Value): Unit =  move(new Coordinate(
      getCoordinates(direction, getX, getY).getX, getCoordinates(direction, getX, getY).getY))
      
  /** Draw the object to the screen */
  override def draw = {
    if (getGrid.getTile(getX, getY).visible && getGrid.getTile(getX, getY).explored && 
        image.getImageWidth == 32 && image.getImageHeight == 32) {
      drawQuadTex(image, x - (getPlayer.getX - 16) * 32, 
          y - (getPlayer.getY - 8) * 32, image.getImageWidth, image.getImageHeight)
    }
    else if (getGrid.getTile(getX, getY).visible && getGrid.getTile(getX, getY).explored) {
      drawQuadTex(image, x - (getPlayer.getX - 16) * 32 - 16, y - (getPlayer.getY - 8) * 32 - 32, 
        image.getImageWidth, image.getImageHeight)
    }
    if (getGrid.getTile(getX, getY).visible && getGrid.getTile(getX, getY).explored) 
      for (extra <- extraToDraw) {
        if (extra.getImageHeight.toInt == 32 && extra.getImageWidth.toInt == 32) 
          drawQuadTex(extra, x - (getPlayer.getX - 16) * 32, y - (getPlayer.getY - 8) * 32, 
              extra.getImageWidth, extra.getImageHeight)
        else if (extra.getImageHeight.toInt == 64 && extra.getImageWidth.toInt == 64) 
          drawQuadTex(extra, x - (getPlayer.getX - 16) * 32 - 16, y - (getPlayer.getY - 7) * 32, 
              extra.getImageWidth, extra.getImageHeight)
      }
  }
  
}

/** TODO */
object MonsterType extends Enumeration with Serializable {

  type Monster = Value
  val RAT, BAT, SNAKEA, SNAKEB, SPIDER, GOBLINA, GOBLINB, HOUND, LIZARDA, LIZARDB, LIZARDC, 
  CROCODILE, SLOTH, DJINN = Value
  
  private var rat: scala.xml.Node = null
  private var bat: scala.xml.Node = null
  private var snakea: scala.xml.Node = null
  private var snakeb: scala.xml.Node = null
  private var spider: scala.xml.Node = null
  private var goblina: scala.xml.Node = null
  private var goblinb: scala.xml.Node = null
  private var hound: scala.xml.Node = null
  private var lizarda: scala.xml.Node = null
  private var lizardb: scala.xml.Node = null
  private var lizardc: scala.xml.Node = null
  private var crocodile: scala.xml.Node = null
  private var sloth: scala.xml.Node = null
  private var djinn: scala.xml.Node = null
  
  private val xml = XML.loadFile("data/monsters.xml")
  
  for (i <- xml.child) i match {
    case o if ((o \ "MonsterType").text == "RAT") => rat = o
    case o if ((o \ "MonsterType").text == "BAT") => bat = o
    case o if ((o \ "MonsterType").text == "SNAKEA") => snakea = o
    case o if ((o \ "MonsterType").text == "SNAKEB") => snakeb = o
    case o if ((o \ "MonsterType").text == "SPIDER") => spider = o
    case o if ((o \ "MonsterType").text == "GOBLINA") => goblina = o
    case o if ((o \ "MonsterType").text == "GOBLINB") => goblinb = o
    case o if ((o \ "MonsterType").text == "HOUND") => hound = o
    case o if ((o \ "MonsterType").text == "LIZARDA") => lizarda = o
    case o if ((o \ "MonsterType").text == "LIZARDB") => lizardb = o
    case o if ((o \ "MonsterType").text == "LIZARDC") => lizardc = o
    case o if ((o \ "MonsterType").text == "CROCODILE") => crocodile = o
    case o if ((o \ "MonsterType").text == "SLOTH") => sloth = o
    case o if ((o \ "MonsterType").text == "DJINN") => djinn = o
    case _ => {}
  }
  
  /** return monster's xml node */
  def getXml(MonsterType: Monster): scala.xml.Node = {
    MonsterType match {
      case t if (t == RAT) => rat
      case t if (t == BAT) => bat
      case t if (t == SNAKEA) => snakea
      case t if (t == SNAKEB) => snakeb
      case t if (t == SPIDER) => spider
      case t if (t == GOBLINA) => goblina
      case t if (t == GOBLINB) => goblinb
      case t if (t == HOUND) => hound
      case t if (t == LIZARDA) => lizarda
      case t if (t == LIZARDB) => lizardb
      case t if (t == LIZARDC) => lizardc
      case t if (t == CROCODILE) => crocodile
      case t if (t == SLOTH) => sloth
      case t if (t == DJINN) => djinn
      case _ => null
    }
  }
  
  /** return chances how monsters occur in game */
  def levelChance(level: Int): Map[Monster, Int] = {
    var chances = Map[Monster, Int]()
    level match {
      case l if (l == 1) => chances = 
        Map(RAT -> 10, BAT -> 50, SNAKEA -> 25, SNAKEB -> 10, SPIDER -> 25, GOBLINA -> 5, 
            GOBLINB -> 1, HOUND -> 2)
      case l if (l == 2) => chances = 
        Map(RAT -> 10, BAT -> 25, SNAKEA -> 25, SNAKEB -> 10, SPIDER -> 25, GOBLINA -> 16, 
            GOBLINB -> 4, HOUND -> 10)
      case l if (l == 3) => chances = 
        Map(RAT -> 10, BAT -> 15, SNAKEA -> 10, SNAKEB -> 2, SPIDER -> 10, GOBLINA -> 20, 
            GOBLINB -> 5, HOUND -> 15, LIZARDA -> 10, LIZARDB -> 4, LIZARDC -> 2, CROCODILE -> 1)
      case l if (l == 4) => chances = 
        Map(RAT -> 10, GOBLINA -> 8, GOBLINB -> 2, HOUND -> 5, LIZARDA -> 20, LIZARDB -> 8, 
            LIZARDC -> 2, CROCODILE -> 5)
      case _ => {chances = Map(BAT -> 100)}
    }
    chances
  }
  
  /** returns texture of the given monster */
  def image(MonsterType: Monster, isPassive: Boolean): Texture = {
    MonsterType match {
      case t if (t == RAT) => if (isPassive) rat2 else rat1
      case t if (t == BAT) => if (isPassive) bat2 else bat1
      case t if (t == SNAKEA) => if (isPassive) snakea2 else snakea1
      case t if (t == SNAKEB) => if (isPassive) snakeb2 else snakeb1
      case t if (t == SPIDER) => if (isPassive) spider2 else spider1
      case t if (t == GOBLINA) => if (isPassive) goblina2 else goblina1
      case t if (t == GOBLINB) => if (isPassive) goblinb2 else goblinb1
      case t if (t == HOUND) => if (isPassive) hound2 else hound1
      case t if (t == LIZARDA) => if (isPassive) lizarda2 else lizarda1
      case t if (t == LIZARDB) => if (isPassive) lizardb2 else lizardb1
      case t if (t == LIZARDC) => if (isPassive) lizardc2 else lizardc1
      case t if (t == CROCODILE) => if (isPassive) crocodile2 else crocodile1
      case t if (t == SLOTH) => if (isPassive) sloth2 else sloth1
      case t if (t == DJINN) => {
        if (isPassive) {
          if (getGrid.shopImageNumbers._1 == 1) djinn12
          else if (getGrid.shopImageNumbers._1 == 2) djinn22
          else djinn32
        }
        else if (getGrid.shopImageNumbers._1 == 1) djinn11
        else if (getGrid.shopImageNumbers._1 == 2) djinn21
        else djinn31
      }
      case _ => missing
    }
  }
  
  /** returns max health of the given monster */
  def maxHP(MonsterType: Monster): Double = {
    if (getXml(MonsterType) != null) (getXml(MonsterType) \ "maxHP").text.toInt
    else 0.0
  }
  
  /** returns damage of the given monster 
   * 
   * Tuple3 includes number of dices, their number of eyes and additional flat bonus 
   * (num of dices, eyes, flat). Examples 2d3+5 = (2, 3, 5) and 1d4 = (1, 4, 0).
   *  */
  def damage(MonsterType: Monster): Double = {
    if (getXml(MonsterType) != null) {
      val a = (getXml(MonsterType) \ "damage").text.split(",")
      roll(a(0).toInt, a(1).toInt) + a(2).toInt
    }
    else 0.0
  }
  
  /** returns armor of the given monster */
  def armor(MonsterType: Monster): Double = {
    if (getXml(MonsterType) != null) (getXml(MonsterType) \ "armor").text.toDouble
    else 0.0
  }
  
  /** returns accuracy of the given monster */
  def accuracy(MonsterType: Monster): Double = {
    if (getXml(MonsterType) != null) (getXml(MonsterType) \ "accuracy").text.toInt
    else 0.0
  }
  
  /** returns critical chance of the given monster */
  def criticalChance(MonsterType: Monster): Double = {
    if (getXml(MonsterType) != null) (getXml(MonsterType) \ "criticalChance").text.toInt
    else 0.0
  }
  
  /** returns dodge chance of the given monster */
  def dodge(MonsterType: Monster): Double = {
    if (getXml(MonsterType) != null) (getXml(MonsterType) \ "dodge").text.toInt
    else 0
  }
  
  /** returns armor pierce of the given monster */
  def armorPierce(MonsterType: Monster): Double = {
    if (getXml(MonsterType) != null) (getXml(MonsterType) \ "armorPierce").text.toInt
    else 0.0
  }
  
  /** returns gold of the given monster */
  def gold(MonsterType: Monster): Double = {
    if (getXml(MonsterType) != null) (getXml(MonsterType) \ "gold").text.toInt
    else 0.0
  }
  
  /** returns experience of the given monster */
  def experience(MonsterType: Monster): Double = {
    if (getXml(MonsterType) != null) (getXml(MonsterType) \ "experience").text.toInt
    else 0.0
  }
  
  /** returns piety of the given monster */
  def piety(MonsterType: Monster): Double = {
    if (getXml(MonsterType) != null) (getXml(MonsterType) \ "piety").text.toInt
    else 0.0
  }
  
  /** monsters might give item drops when dying */
  def drop(MonsterType: Monster, x: Int, y: Int) {
    MonsterType match {
      case t if (t == RAT) => if (rnd.nextInt(2) == 0) new Useable(x, y, ItemType.RATMEAT, false)
      case t if (t == GOBLINA) => if (rnd.nextInt(3) == 0) new Useable(x, y, ItemType.SMALLHEALPOTION, false)
      case t if (t == GOBLINB) => if (rnd.nextInt(2) == 0) new Useable(x, y, ItemType.SMALLHEALPOTION, false)
      case _ => {}
    }
  }
  
  /** returns name of the given monster */
  def name(MonsterType: Monster): String = {
    if (getXml(MonsterType) != null) (getXml(MonsterType) \ "name").text
    else "Unknown monster name"
  }
  
  /** returns description of the given monster */
  def description(MonsterType: Monster): String = {
    if (getXml(MonsterType) != null) (getXml(MonsterType) \ "description").text
    else "Unknown monster description"
  }
}

/** All of the game's items will be under this trait */
trait Item extends Object with Serializable {
  
  var inShop: Boolean
  var equipped: Boolean
  val itemType: ItemType.Value
  def image = ItemType.imageGround(itemType)
  def imageEquipped = ItemType.imageEquipped(itemType)
  def price: Double = ItemType.price(itemType)
  def armor = ItemType.armor(itemType)
  def shieldArmor = ItemType.shieldArmor(itemType)
  def weight: Double = ItemType.weight(itemType)
  def blockChance: Double = ItemType.blockChance(itemType)
  def armorPiercing: Double = ItemType.armorPiercing(itemType)
  def accuracy: Double = ItemType.accuracy(itemType)
  def critChance: Double = ItemType.critChance(itemType)
  def zeal: Double = ItemType.zeal(itemType)
  def humility: Double = ItemType.humility(itemType)
  def temperance: Double = ItemType.temperance(itemType)
  def kindness: Double = ItemType.kindness(itemType)
  def patience: Double = ItemType.patience(itemType)
  def charity: Double = ItemType.charity(itemType)
  def diligence: Double = ItemType.diligence(itemType)
  def health: Double = ItemType.health(itemType)
  def viewRange: Double = ItemType.viewRange(itemType)
  def prayChance: Double = ItemType.prayChance(itemType)
  def xpModifier: Double = ItemType.xpModifier(itemType)
  def goldModifier: Double = ItemType.goldModifier(itemType)
  def pietyModifier: Double = ItemType.pietyModifier(itemType)
  def dodge: Double = ItemType.dodge(itemType)
  def damage: Double
  
  def buy = {
    getPlayer.gold -= price
    inShop = false
    pickUp
  }
  
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
      case s if (s == "weapon") => {
        if (ItemType.is2h(itemType)) {
          if (getPlayer.slotWeapon != null) getPlayer.slotWeapon.unequip
          if (getPlayer.slotShield != null) getPlayer.slotShield.unequip
        }
        else if (getPlayer.slotWeapon != null) getPlayer.slotWeapon.unequip
      }
      case s if (s == "shield") => {
        if (ItemType.is2h(getPlayer.slotWeapon.itemType)) {
          if (getPlayer.slotWeapon != null) getPlayer.slotWeapon.unequip
          if (getPlayer.slotShield != null) getPlayer.slotShield.unequip
        }
        else if (getPlayer.slotShield != null) getPlayer.slotShield.unequip
      }
      case s if (s == "armor") => if (getPlayer.slotArmor != null) getPlayer.slotArmor.unequip
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
  var blockMovement = false
  var blockVision = false
  var equipped: Boolean = isEquipped
  var inShop = false
  
  def damage: Double = roll(ItemType.damage(itemType)._1, ItemType.damage(itemType)._2) + ItemType.damage(itemType)._3
  
  def infoBox: Buffer[Buffer[(String, Color)]] = {
    Buffer[Buffer[(String, Color)]](Buffer(("Name: ", Color.black), (name, Color.black)))
  }
  
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

/** TODO */
object ItemType extends Enumeration with Serializable {

  type Item = Value
  val KNIFE, ROBES, IRONARMOR, STEELSWORD, WOODENSHIELD, RATMEAT, SMALLHEALPOTION, BATTLEAXE, 
  CLOTH1, CLOTH2, IRONSHIELD, KATANA, LARGESHIELD, SHORTSWORD, SMALLSHIELD, STEELARMOR, CLAYMORE, 
  DUALDAGGER, GOLDARMOR, MAGICSWORD, MONODAGGER, VIKINGARMOR, BUFFPOTION, CHARITYPOTION, 
  DILIGENCEPOTION, HUMILITYPOTION, IMMUNITYPOTION, KINDNESSPOTION, PATIENCEPOTION, TEMPERANCEPOTION, 
  TIMEPOTION, VISIONPOTION, ZEALPOTION = Value
  
  private var knife: scala.xml.Node = null
  private var robes: scala.xml.Node = null
  private var ironArmor: scala.xml.Node = null
  private var steelSword: scala.xml.Node = null
  private var woodenShield: scala.xml.Node = null
  private var ratMeat: scala.xml.Node = null
  private var smallHealingPotion: scala.xml.Node = null
  private var battleAxe: scala.xml.Node = null
  private var cloth1: scala.xml.Node = null
  private var cloth2: scala.xml.Node = null
  private var ironShield: scala.xml.Node = null
  private var katana: scala.xml.Node = null
  private var largeShield: scala.xml.Node = null
  private var shortSword: scala.xml.Node = null
  private var smallShield: scala.xml.Node = null
  private var steelArmor: scala.xml.Node = null
  private var claymore: scala.xml.Node = null
  private var dualDagger: scala.xml.Node = null
  private var goldArmor: scala.xml.Node = null
  private var magicSword: scala.xml.Node = null
  private var monoDagger: scala.xml.Node = null
  private var vikingArmor: scala.xml.Node = null
  private var buffpotion: scala.xml.Node = null
  private var charitypotion: scala.xml.Node = null
  private var diligencepotion: scala.xml.Node = null
  private var humilitypotion: scala.xml.Node = null
  private var immunitypotion: scala.xml.Node = null
  private var kindnesspotion: scala.xml.Node = null
  private var patiencepotion: scala.xml.Node = null
  private var temperancepotion: scala.xml.Node = null
  private var timepotion: scala.xml.Node = null
  private var visionpotion: scala.xml.Node = null
  private var zealpotion: scala.xml.Node = null  
  
  private val xml = XML.loadFile("data/items.xml")
  
  for (i <- xml.child) i match {
    case o if ((o \ "ItemType").text == "KNIFE") => knife = o
    case o if ((o \ "ItemType").text == "ROBES") => robes = o
    case o if ((o \ "ItemType").text == "IRONARMOR") => ironArmor = o
    case o if ((o \ "ItemType").text == "STEELSWORD") => steelSword = o
    case o if ((o \ "ItemType").text == "WOODENSHIELD") => woodenShield = o
    case o if ((o \ "ItemType").text == "RATMEAT") => ratMeat = o
    case o if ((o \ "ItemType").text == "SMALLHEALPOTION") => smallHealingPotion = o
    case o if ((o \ "ItemType").text == "BATTLEAXE") => battleAxe = o
    case o if ((o \ "ItemType").text == "CLOTH1") => cloth1 = o
    case o if ((o \ "ItemType").text == "CLOTH2") => cloth2 = o
    case o if ((o \ "ItemType").text == "IRONSHIELD") => ironShield = o
    case o if ((o \ "ItemType").text == "KATANA") => katana = o
    case o if ((o \ "ItemType").text == "LARGESHIELD") => largeShield = o
    case o if ((o \ "ItemType").text == "SHORTSWORD") => shortSword = o
    case o if ((o \ "ItemType").text == "SMALLSHIELD") => smallShield = o
    case o if ((o \ "ItemType").text == "STEELARMOR") => steelArmor = o
    case o if ((o \ "ItemType").text == "CLAYMORE") => claymore = o
    case o if ((o \ "ItemType").text == "DUALDAGGER") => dualDagger = o
    case o if ((o \ "ItemType").text == "GOLDARMOR") => goldArmor = o
    case o if ((o \ "ItemType").text == "MAGICSWORD") => magicSword = o
    case o if ((o \ "ItemType").text == "MONODAGGER") => monoDagger = o
    case o if ((o \ "ItemType").text == "VIKINGARMOR") => vikingArmor = o
    case o if ((o \ "ItemType").text == "BUFFPOTION") => buffpotion = o
    case o if ((o \ "ItemType").text == "CHARITYPOTION") => charitypotion = o
    case o if ((o \ "ItemType").text == "DILIGENCEPOTION") => diligencepotion = o
    case o if ((o \ "ItemType").text == "HUMILITYPOTION") => humilitypotion = o
    case o if ((o \ "ItemType").text == "IMMUNITYPOTION") => immunitypotion = o
    case o if ((o \ "ItemType").text == "KINDNESSPOTION") => kindnesspotion = o
    case o if ((o \ "ItemType").text == "PATIENCEPOTION") => patiencepotion = o
    case o if ((o \ "ItemType").text == "TEMPERANCEPOTION") => temperancepotion = o
    case o if ((o \ "ItemType").text == "TIMEPOTION") => timepotion = o
    case o if ((o \ "ItemType").text == "VISIONPOTION") => visionpotion = o
    case o if ((o \ "ItemType").text == "ZEALPOTION") => zealpotion = o
    case _ => {}
  }
  
  /** return item's xml node */
  def getXml(ItemType: Item): scala.xml.Node = {
    ItemType match {
      case t if (t == KNIFE) => knife
      case t if (t == ROBES) => robes
      case t if (t == IRONARMOR) => ironArmor
      case t if (t == STEELSWORD) => steelSword
      case t if (t == WOODENSHIELD) => woodenShield
      case t if (t == RATMEAT) => ratMeat
      case t if (t == SMALLHEALPOTION) => smallHealingPotion
      case t if (t == BATTLEAXE) => battleAxe
      case t if (t == CLOTH1) => cloth1
      case t if (t == CLOTH2) => cloth2
      case t if (t == IRONSHIELD) => ironShield
      case t if (t == KATANA) => katana
      case t if (t == LARGESHIELD) => largeShield
      case t if (t == SHORTSWORD) => shortSword
      case t if (t == SMALLSHIELD) => smallShield
      case t if (t == STEELARMOR) => steelArmor
      case t if (t == CLAYMORE) => claymore
      case t if (t == DUALDAGGER) => dualDagger
      case t if (t == GOLDARMOR) => goldArmor
      case t if (t == MAGICSWORD) => magicSword
      case t if (t == MONODAGGER) => monoDagger
      case t if (t == VIKINGARMOR) => vikingArmor
      case t if (t == BUFFPOTION) => buffpotion
      case t if (t == CHARITYPOTION) => charitypotion
      case t if (t == DILIGENCEPOTION) => diligencepotion
      case t if (t == HUMILITYPOTION) => humilitypotion
      case t if (t == IMMUNITYPOTION) => immunitypotion
      case t if (t == KINDNESSPOTION) => kindnesspotion
      case t if (t == PATIENCEPOTION) => patiencepotion
      case t if (t == TEMPERANCEPOTION) => temperancepotion
      case t if (t == TIMEPOTION) => timepotion
      case t if (t == VISIONPOTION) => visionpotion
      case t if (t == ZEALPOTION) => zealpotion
      case _ => null
    }
  }
  
  /** return chances how items occur in game */
  def levelChance(level: Int): Map[Item, Int] = {
    var chances = Map[Item, Int]()
    level match {
      case l if (l == 1) => chances = 
        Map(WOODENSHIELD -> 2, IRONARMOR -> 1, SMALLHEALPOTION -> 5, SHORTSWORD -> 1, 
            SMALLSHIELD -> 1, MONODAGGER -> 2, DUALDAGGER -> 1) //LEATHERARMOR -> 2
      case l if (l == 2) => chances = 
        Map(WOODENSHIELD -> 1, IRONARMOR -> 2, SMALLHEALPOTION -> 5, LARGESHIELD -> 1, 
            SHORTSWORD -> 2, SMALLSHIELD -> 2, VIKINGARMOR -> 1, IRONARMOR -> 2, MONODAGGER -> 1, 
            DUALDAGGER -> 1) // LEATHERARMOR -> 1, SPEAR -> 1
      case l if (l == 3) => chances = 
        Map(DUALDAGGER -> 1, SHORTSWORD -> 1, IRONARMOR -> 1, SMALLHEALPOTION -> 5, 
            BATTLEAXE -> 1, IRONSHIELD -> 1, LARGESHIELD -> 2, SHORTSWORD -> 1, SMALLSHIELD -> 1, 
            STEELARMOR -> 1, VIKINGARMOR -> 2, IRONARMOR -> 1) // HEATERSHIELD -> 1, SPEAR -> 2
      case l if (l == 4) => chances = 
        Map(STEELSWORD -> 2, IRONARMOR -> 1, SMALLHEALPOTION -> 5, BATTLEAXE -> 2, IRONSHIELD -> 1, 
            LARGESHIELD -> 1, SHORTSWORD -> 1, VIKINGARMOR -> 1, GOLDARMOR -> 1, STEELARMOR -> 2, 
            CLAYMORE -> 2) // HEATERSHIELD -> 2, SPEAR -> 1
      case l if (l == 5) => chances = 
        Map(STEELSWORD -> 2, IRONARMOR -> 1, SMALLHEALPOTION -> 5, BATTLEAXE -> 1, IRONSHIELD -> 2, 
            GOLDARMOR -> 2, STEELARMOR -> 1, CLAYMORE -> 2) // HEATERSHIELD -> 1
      case _ => {chances = Map(KNIFE -> 100)}
    }
    chances
  }
  
  /** return chances how items occur in game */
  def shopChance(level: Int): Map[Item, Int] = {
    var chances = levelChance(level)
    level match {
      case l if (l == 1) => chances += (CLOTH1 -> 2, CLOTH2 -> 2)
      case l if (l == 2) => chances += (CLOTH1 -> 2, CLOTH2 -> 1)
      case l if (l == 3) => chances += (CLOTH1 -> 1, CLOTH2 -> 1)
      case l if (l == 4) => chances += (CLOTH1 -> 1)
      case l if (l == 5) => chances += (BUFFPOTION -> 1, CHARITYPOTION -> 1, DILIGENCEPOTION -> 1, 
          HUMILITYPOTION -> 1, IMMUNITYPOTION -> 1, KINDNESSPOTION -> 1, PATIENCEPOTION -> 1, 
          TEMPERANCEPOTION -> 1, TIMEPOTION -> 1, VISIONPOTION -> 1, ZEALPOTION -> 1)
      case _ => {chances = Map(KNIFE -> 100)}
    }
    chances
  }
  
  /** returns slot of the given equipment */
  def slot(ItemType: Item): String = {
    if (getXml(ItemType) != null) (getXml(ItemType) \ "slot").text
    else ""
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
      case t if (t == BATTLEAXE) => battleAxeG
      case t if (t == CLOTH1) => cloth1G
      case t if (t == CLOTH2) => cloth2G
      case t if (t == IRONSHIELD) => ironShieldG
      case t if (t == KATANA) => katanaG
      case t if (t == LARGESHIELD) => largeShieldG
      case t if (t == SHORTSWORD) => shortSwordG
      case t if (t == SMALLSHIELD) => smallShieldG
      case t if (t == STEELARMOR) => steelArmorG
      case t if (t == CLAYMORE) => claymoreG
      case t if (t == DUALDAGGER) => dualDaggerG
      case t if (t == GOLDARMOR) => goldArmorG
      case t if (t == MAGICSWORD) => magicSwordG
      case t if (t == MONODAGGER) => monoDaggerG
      case t if (t == VIKINGARMOR) => vikingArmorG
      case t if (t == BUFFPOTION) => Helpers.buffpotion
      case t if (t == CHARITYPOTION) => Helpers.charitypotion
      case t if (t == DILIGENCEPOTION) => Helpers.diligencepotion
      case t if (t == HUMILITYPOTION) => Helpers.humilitypotion
      case t if (t == IMMUNITYPOTION) => Helpers.immunitypotion
      case t if (t == KINDNESSPOTION) => Helpers.kindnesspotion
      case t if (t == PATIENCEPOTION) => Helpers.patiencepotion
      case t if (t == TEMPERANCEPOTION) => Helpers.temperancepotion
      case t if (t == TIMEPOTION) => Helpers.timepotion
      case t if (t == VISIONPOTION) => Helpers.visionpotion
      case t if (t == ZEALPOTION) => Helpers.zealpotion
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
      case t if (t == BATTLEAXE) => battleAxeE
      case t if (t == CLOTH1) => cloth1E
      case t if (t == CLOTH2) => cloth2E
      case t if (t == IRONSHIELD) => ironShieldE
      case t if (t == KATANA) => katanaE
      case t if (t == LARGESHIELD) => largeShieldE
      case t if (t == SHORTSWORD) => shortSwordE
      case t if (t == SMALLSHIELD) => smallShieldE
      case t if (t == STEELARMOR) => steelArmorE
      case t if (t == CLAYMORE) => claymoreE
      case t if (t == DUALDAGGER) => dualDaggerE
      case t if (t == GOLDARMOR) => goldArmorE
      case t if (t == MAGICSWORD) => magicSwordE
      case t if (t == MONODAGGER) => monoDaggerE
      case t if (t == VIKINGARMOR) => vikingArmorE
      case t if (t == BUFFPOTION) => Helpers.buffpotion
      case t if (t == CHARITYPOTION) => Helpers.charitypotion
      case t if (t == DILIGENCEPOTION) => Helpers.diligencepotion
      case t if (t == HUMILITYPOTION) => Helpers.humilitypotion
      case t if (t == IMMUNITYPOTION) => Helpers.immunitypotion
      case t if (t == KINDNESSPOTION) => Helpers.kindnesspotion
      case t if (t == PATIENCEPOTION) => Helpers.patiencepotion
      case t if (t == TEMPERANCEPOTION) => Helpers.temperancepotion
      case t if (t == TIMEPOTION) => Helpers.timepotion
      case t if (t == VISIONPOTION) => Helpers.visionpotion
      case t if (t == ZEALPOTION) => Helpers.zealpotion
      case _ => missing
    }
  }

  /** returns boolean about item being held with two hands */
  def is2h(ItemType: Item): Boolean = {
    if (getXml(ItemType) != null && (getXml(ItemType) \ "is2h").text == "true") true
    else false
  }
  
  /** returns armor of the given equipment */
  def armor(ItemType: Item): Double = {
    if (getXml(ItemType) != null && (getXml(ItemType) \ "armor").text != "") 
      (getXml(ItemType) \ "armor").text.toDouble
    else 0.0
  }
  
  /** returns armor of the given shield */
  def shieldArmor(ItemType: Item): Double = {
    if (getXml(ItemType) != null && (getXml(ItemType) \ "shieldArmor").text != "") 
      (getXml(ItemType) \ "shieldArmor").text.toDouble
    else 0.0
  }
  
  /** returns weight of the given equipment */
  def weight(ItemType: Item): Double = {
    if (getXml(ItemType) != null && (getXml(ItemType) \ "weight").text != "") 
      (getXml(ItemType) \ "weight").text.toDouble
    else 0
  }
  
  /** returns block chance of the given equipment */
  def blockChance(ItemType: Item): Double = {
    if (getXml(ItemType) != null && (getXml(ItemType) \ "blockChance").text != "") 
      (getXml(ItemType) \ "blockChance").text.toDouble
    else 0
  }
  
  /** Return damage of the given equipment.
   * 
   * Tuple3 includes number of dices, their number of eyes and additional flat bonus 
   * (num of dices, eyes, flat). Examples 2d3+5 = (2, 3, 5) and 1d4 = (1, 4, 0).
   */
  def damage(ItemType: Item): Tuple3[Int, Int, Int] = {
    if (getXml(ItemType) != null && (getXml(ItemType) \ "damage").text != "") {
      val a = (getXml(ItemType) \ "damage").text.split(",")
      (a(0).toInt, a(1).toInt, a(2).toInt)
    }
    else (0, 0, 0)
  }
  
  /** returns armor piercing of the given equipment */
  def armorPiercing(ItemType: Item): Double = {
    if (getXml(ItemType) != null && (getXml(ItemType) \ "armorPiercing").text != "") 
      (getXml(ItemType) \ "armorPiercing").text.toDouble
    else 0
  }
  
  /** returns accuracy of the given equipment */
  def accuracy(ItemType: Item): Double = {
    if (getXml(ItemType) != null && (getXml(ItemType) \ "accuracy").text != "") 
      (getXml(ItemType) \ "accuracy").text.toDouble
    else 0
  }
  
  /** returns critical chance of the given equipment */
  def critChance(ItemType: Item): Double = {
    if (getXml(ItemType) != null && (getXml(ItemType) \ "critChance").text != "") 
      (getXml(ItemType) \ "critChance").text.toDouble
    else 0
  }
  
  /** returns price of the given equipment */
  def price(ItemType: Item): Double = {
    if (getXml(ItemType) != null && (getXml(ItemType) \ "price").text != "") 
      (getXml(ItemType) \ "price").text.toDouble
    else 0
  }
  
  /** returns name of the given equipment */
  def name(ItemType: Item): String = {
    if (getXml(ItemType) != null && (getXml(ItemType) \ "name").text != "") 
      (getXml(ItemType) \ "name").text
    else "Unknown item name"
  }
  
  /** returns description of the given equipment */
  def description(ItemType: Item): String = {
    if (getXml(ItemType) != null && (getXml(ItemType) \ "description").text != "") 
      (getXml(ItemType) \ "description").text
    else "Unknown item description"
  }
  
  
  def zeal(ItemType: Item): Double = {
    if (getXml(ItemType) != null && (getXml(ItemType) \ "zeal").text != "") 
      (getXml(ItemType) \ "zeal").text.toDouble
    else 0.0
  }
  
  def humility(ItemType: Item): Double = {
    if (getXml(ItemType) != null && (getXml(ItemType) \ "humility").text != "") 
      (getXml(ItemType) \ "humility").text.toDouble
    else 0.0
  }
  
  def temperance(ItemType: Item): Double = {
    if (getXml(ItemType) != null && (getXml(ItemType) \ "temperance").text != "") 
      (getXml(ItemType) \ "temperance").text.toDouble
    else 0.0
  }
  
  def kindness(ItemType: Item): Double = {
    if (getXml(ItemType) != null && (getXml(ItemType) \ "kindness").text != "") 
      (getXml(ItemType) \ "kindness").text.toDouble
    else 0.0
  }
  
  def patience(ItemType: Item): Double = {
    if (getXml(ItemType) != null && (getXml(ItemType) \ "patience").text != "") 
      (getXml(ItemType) \ "patience").text.toDouble
    else 0.0
  }
  
  def charity(ItemType: Item): Double = {
    if (getXml(ItemType) != null && (getXml(ItemType) \ "charity").text != "") 
      (getXml(ItemType) \ "charity").text.toDouble
    else 0.0
  }
  
  def diligence(ItemType: Item): Double = {
    if (getXml(ItemType) != null && (getXml(ItemType) \ "diligence").text != "") 
      (getXml(ItemType) \ "diligence").text.toDouble
    else 0.0
  }
  
  def health(ItemType: Item): Double = {
    if (getXml(ItemType) != null && (getXml(ItemType) \ "health").text != "") 
      (getXml(ItemType) \ "health").text.toDouble
    else 0.0
  }
  
  def viewRange(ItemType: Item): Double = {
    if (getXml(ItemType) != null && (getXml(ItemType) \ "viewRange").text != "") 
      (getXml(ItemType) \ "viewRange").text.toDouble
    else 0.0
  }
  
  def prayChance(ItemType: Item): Double = {
    if (getXml(ItemType) != null && (getXml(ItemType) \ "prayChance").text != "") 
      (getXml(ItemType) \ "prayChance").text.toDouble
    else 0.0
  }
  
  def xpModifier(ItemType: Item): Double = {
    if (getXml(ItemType) != null && (getXml(ItemType) \ "xpModifier").text != "") 
      (getXml(ItemType) \ "xpModifier").text.toDouble
    else 1.0
  }
  
  def goldModifier(ItemType: Item): Double = {
    if (getXml(ItemType) != null && (getXml(ItemType) \ "goldModifier").text != "") 
      (getXml(ItemType) \ "goldModifier").text.toDouble
    else 1.0
  }
  
  def pietyModifier(ItemType: Item): Double = {
    if (getXml(ItemType) != null && (getXml(ItemType) \ "pietyModifier").text != "") 
      (getXml(ItemType) \ "pietyModifier").text.toDouble
    else 1.0
  }
  
  def dodge(ItemType: Item): Double = {
    if (getXml(ItemType) != null && (getXml(ItemType) \ "dodge").text != "") 
      (getXml(ItemType) \ "dodge").text.toDouble
    else 0.0
  }
  
}

/** Player usable consumables */
class Useable(startX: Int, startY: Int, val itemType: ItemType.Value, isEquipped: Boolean) extends Item with Serializable {

  val rnd = getRnd
  var name = ItemType.name(itemType)
  var description = ItemType.description(itemType)
  var x = startX * 32
  var y = startY * 32
  var blockMovement = false
  var blockVision = false
  var inShop = false
  var equipped = isEquipped
  def damage: Double = roll(ItemType.damage(itemType)._1, ItemType.damage(itemType)._2) + ItemType.damage(itemType)._3
  
  if (!equipped) {
    init
    getUseableList.append(this)
  }
  
  def infoBox: Buffer[Buffer[(String, Color)]] = {
    Buffer[Buffer[(String, Color)]](Buffer(("Name: ", Color.black), (name, Color.black)))
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
        if (getPlayer.piety > 0) getPlayer.piety = getPlayer.piety * 0.9 - 10
        else getPlayer.piety * 1.1 - 10
        addLog(getPlayer.name.toUpperCase.head + getPlayer.name.tail + " eats " + name + ".")
      }
      case i if (i == ItemType.SMALLHEALPOTION) => {
        getPlayer.effectList = getPlayer.effectList :+ new smallHeal(roll(5)+5, getPlayer)
        addLog(getPlayer.name.toUpperCase.head + getPlayer.name.tail + " drinks " + name + ".")
      }
      case i if (i == ItemType.BUFFPOTION) => {
        getPlayer.effectList = getPlayer.effectList :+ new buff(roll(10)+10, getPlayer)
        addLog(getPlayer.name.toUpperCase.head + getPlayer.name.tail + " drinks " + name + ".")
      }
      case i if (i == ItemType.IMMUNITYPOTION) => {
        getPlayer.effectList = getPlayer.effectList :+ new immunity(roll(10)+10, getPlayer)
        addLog(getPlayer.name.toUpperCase.head + getPlayer.name.tail + " drinks " + name + ".")
      }
      case i if (i == ItemType.TIMEPOTION) => {
        getPlayer.effectList = getPlayer.effectList :+ new timestop(roll(10)+10, getPlayer)
        addLog(getPlayer.name.toUpperCase.head + getPlayer.name.tail + " drinks " + name + ".")
      }
      case i if (i == ItemType.VISIONPOTION) => {
        getPlayer.effectList = getPlayer.effectList :+ new vision(roll(20)+20, getPlayer)
        addLog(getPlayer.name.toUpperCase.head + getPlayer.name.tail + " drinks " + name + ".")
      }
      case i if (i == ItemType.CHARITYPOTION) => {
        getPlayer.charity += 1
        addLog(getPlayer.name.toUpperCase.head + getPlayer.name.tail + " drinks " + name + ".")
      }
      case i if (i == ItemType.DILIGENCEPOTION) => {
        getPlayer.diligence += 1
        addLog(getPlayer.name.toUpperCase.head + getPlayer.name.tail + " drinks " + name + ".")
      }
      case i if (i == ItemType.HUMILITYPOTION) => {
        getPlayer.humility += 1
        addLog(getPlayer.name.toUpperCase.head + getPlayer.name.tail + " drinks " + name + ".")
      }
      case i if (i == ItemType.KINDNESSPOTION) => {
        getPlayer.kindness += 1
        addLog(getPlayer.name.toUpperCase.head + getPlayer.name.tail + " drinks " + name + ".")
      }
      case i if (i == ItemType.PATIENCEPOTION) => {
        getPlayer.patience += 1
        addLog(getPlayer.name.toUpperCase.head + getPlayer.name.tail + " drinks " + name + ".")
      }
      case i if (i == ItemType.TEMPERANCEPOTION) => {
        getPlayer.temperance += 1
        addLog(getPlayer.name.toUpperCase.head + getPlayer.name.tail + " drinks " + name + ".")
      }
      case i if (i == ItemType.ZEALPOTION) => {
        getPlayer.zeal += 1
        addLog(getPlayer.name.toUpperCase.head + getPlayer.name.tail + " drinks " + name + ".")
      }
      case _ => {}
    }
    getPlayer.slotUseable = null
    equipped = false
    x = -100
    y = -100
  }
}