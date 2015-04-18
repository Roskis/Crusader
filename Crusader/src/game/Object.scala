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
  def accuracy: Int
  def ap: Double
  def crit: Int
  def move(coord: Coordinate): Unit
  
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
  
  /** Amount of piety defines view range */
  def viewRadius: Int = {
    var radius = (piety/100).toInt
    if (radius > 5) radius = 5
    else if (radius < - 2) radius = -2
    radius += 5
    var visionBuff = false
    for (effect <- effectList) if (effect.isInstanceOf[vision]) visionBuff = true
    if (visionBuff) radius += 3
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
        case _ => {println("prayer not found")}
      }
    }
    else addLog("You pray.")
    if (getPlayer.piety > 0) getPlayer.piety -= (getPlayer.piety*0.05 + 5)
    else getPlayer.piety -= rnd.nextInt(5)+6
  }
  
  /** Modifier applied to prays */
  def prayChance = {
    var chance:Double = 10
    chance += charity*2.5
    if (getX == getGrid.getAltar.getX && getY == getGrid.getAltar.getY && getLevel%5 == 0) chance += 40
    else if (getX == getGrid.getAltar.getX && getY == getGrid.getAltar.getY) chance += 20
    else if (getLevel%5 == 0) chance += 10
    var isBuffed = false
    for (effect <- effectList) if (effect.isInstanceOf[buff]) isBuffed = true
    if (isBuffed) chance += 20
    chance
  }
  
  /** Modifier applied to all expirience gained */
  def giveXP(amount: Double) = experience += amount * (1+0.1*diligence)
  
  /** Modifier applied to all gold gained */
  def giveGold(amount: Double) = gold += amount * (1+0.1*diligence)
  
  /** Modifier applied to all piety gained */
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
    var weapondmg = roll(slotWeapon.damage._1, slotWeapon.damage._2) + slotWeapon.damage._3
    var isBuffed = false
    for (effect <- effectList) if (effect.isInstanceOf[buff]) isBuffed = true
    if (isBuffed) weapondmg += roll(3)
    if (crit) weapondmg += (zeal+2) + roll(zeal+2)
    else weapondmg += roll(zeal+2)
    weapondmg
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
    var isImmune = false
    for (effect <- effectList) if (effect.isInstanceOf[immunity]) isImmune = true
    var effectiveArmor = armor
    if (rnd.nextInt(100) <= blockChance) effectiveArmor += shieldArmor
    if (effectiveArmor < 0) effectiveArmor = 0
    var effectiveDamage = (damage - effectiveArmor)
    if (effectiveDamage < 0) effectiveDamage = 0
    if (!isImmune) {
      health -= effectiveDamage
      addLog(attacker.name.toUpperCase.head + attacker.name.tail + " deals " + 
          effectiveDamage.toInt.toString + " damage to " + name + ".")
    }
    else {
      addLog(name.toUpperCase.head + name.tail + " is immune to attacks.")
    }
  }
  
  def armor: Double = {
    val patienceBonus: Double = {
      if (patience > 7) 2
      else if (patience > 5) 1.5
      else if (patience > 3) 1
      else if (patience > 1) 0.5
      else 0
    }
    var isBuffed = false
    for (effect <- effectList) if (effect.isInstanceOf[buff]) isBuffed = true
    if (isBuffed) slotArmor.armor + patienceBonus + 1
    else slotArmor.armor + patienceBonus
  }
  
  /** Armor poercing of weapon used */
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
  def dodge: Int = {
    var isBuffed = false
    for (effect <- effectList) if (effect.isInstanceOf[buff]) isBuffed = true
    if (isBuffed) 100 - totalWeight + (humility+2)*2
    else 100 - totalWeight + humility*2
  }
  
  /** Return accuracy of player */
  def accuracy: Int = {
    var blinded = false
    for (effect <- effectList) if (effect.isInstanceOf[blind]) blinded = true
    if (slotWeapon != null && !blinded) slotWeapon.accuracy + temperance*2
    else if (blinded) 10 + temperance*2
    else 100 + temperance*2
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
            if (item.inShop) addLog(item.name.toUpperCase.head + item.name.tail + " is " + item.price + " gold.")
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
  var name = objectName
  var description = objectDescription
  var x = startX * 32
  var y = startY * 32
  def image = PassiveType.image(passiveType)
  var blockMovement = false
  var blockVision = false
  
  init
  getPassiveObjectList.append(this)
  
}

/** TODO */
object PassiveType extends Enumeration with Serializable {

  type Passive = Value
  val ALTAR1, ALTAR2, DJINN, BIGTREE1, TREE1, ROCK1, ROCK2 = Value

  /** returns texture of the given monster */
  def image(PassiveType: Passive): Texture = {
    PassiveType match {
      case t if (t == ALTAR1) => altar1
      case t if (t == ALTAR2) => altar2
      case t if (t == DJINN) => djinn1
      case t if (t == BIGTREE1) => bigTree1
      case t if (t == TREE1) => tree1
      case t if (t == ROCK1) => rock1
      case t if (t == ROCK2) => rock2
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
  var dodge = MonsterType.dodge(mType)
  var blockChance = 0
  var shieldArmor = 0
  var usedAbility = false
  var effectList = Buffer[Effect]()
  var spellcd = 1
  var spellchannel = 0
  var extraToDraw = List[Texture]()
  
  init
  getMonsterList.append(this)
  
  /** Accuracy of the monster */
  def accuracy = {
    var blinded = false
    for (effect <- effectList) if (effect.isInstanceOf[blind]) blinded = true
    if (!blinded) MonsterType.accuracy(mType)
    else 10
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
  def takeDamage(damage: Int, armorPierce: Double, attacker: Object) = {
    updateLastMonster(this)
    var effectiveArmor = armor
    if (rnd.nextInt(100) <= blockChance) effectiveArmor += shieldArmor
    if (effectiveArmor < 0) effectiveArmor = 0
    var effectiveDamage = (damage - effectiveArmor)
    if (effectiveDamage < 0) effectiveDamage = 0
    health -= effectiveDamage
    addLog(attacker.name.toUpperCase.head + attacker.name.tail + " deals " + 
        effectiveDamage.toInt.toString + " damage to " + name.toUpperCase.head + name.tail + ".")
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
        case m if (m == MonsterType.SNAKE) => snakeAI
        case m if (m == MonsterType.LIZARDC) => lizardMageAI
        case m if (m == MonsterType.SLOTH) => slothAI
        case m if (m == MonsterType.SPIDER) => spiderAI
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
    if (distance(getPlayer) > 6 && mode == "passive") {}
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
    if (distance(getPlayer) > 3 && mode == "passive") {
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
    if (distance(getPlayer) > 7 && mode == "passive") {}
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
    else {
      val goto = getDirection(new Coordinate(this.getX, this.getY), new Coordinate(getPlayer.getX, getPlayer.getY))
      if (getGrid.getTile(getCoordinates(goto, getX, getY)).blockMovement) {
        if (rnd.nextBoolean) move(getAdjacentDirections(goto)._1)
        else move(getAdjacentDirections(goto)._2)
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
      else if (distance(coord) < 2 && !getGrid.getTile(coord.getX, coord.getY).blockMovement && boo) {
        changePosition(coord.getX, coord.getY)
      }
    }
  }
  
  /** Move the object to given direction */
  def move(direction: Direction.Value): Unit =  move(new Coordinate(
      getCoordinates(direction, getX, getY).getX, getCoordinates(direction, getX, getY).getY))
  
  /** Draw the object to the screen */
  override def draw = {
    if (getGrid.getTile(getX, getY).visible && getGrid.getTile(getX, getY).explored) {
      drawQuadTex(image, x - (getPlayer.getX - 16) * 32, 
          y - (getPlayer.getY - 8) * 32, image.getImageWidth, image.getImageHeight)
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
}

/** TODO */
object MonsterType extends Enumeration with Serializable {

  type Monster = Value
  val RAT, BAT, SNAKE, SPIDER, GOBLINA, GOBLINB, HOUND, LIZARDA, LIZARDB, LIZARDC, CROCODILE, 
  SLOTH = Value
  
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
  def image(MonsterType: Monster, isPassive: Boolean): Texture = {
    MonsterType match {
      case t if (t == RAT) => if (isPassive) rat2 else rat1
      case t if (t == BAT) => if (isPassive) bat2 else bat1
      case t if (t == SNAKE) => if (isPassive) snake2 else snake1
      case t if (t == SPIDER) => if (isPassive) spider2 else spider1
      case t if (t == GOBLINA) => if (isPassive) goblina2 else goblina1
      case t if (t == GOBLINB) => if (isPassive) goblinb2 else goblinb1
      case t if (t == HOUND) => if (isPassive) hound2 else hound1
      case t if (t == LIZARDA) => if (isPassive) lizarda2 else lizarda1
      case t if (t == LIZARDB) => if (isPassive) lizardb2 else lizardb1
      case t if (t == LIZARDC) => if (isPassive) lizardc2 else lizardc1
      case t if (t == CROCODILE) => if (isPassive) crocodile2 else crocodile1
      case t if (t == SLOTH) => if (isPassive) sloth2 else sloth1
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
      case t if (t == SLOTH) => 50
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
      case t if (t == SLOTH) => (3, 3, 1)
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
      case t if (t == SLOTH) => 2
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
      case t if (t == SLOTH) => 50
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
      case t if (t == LIZARDC) => 5
      case t if (t == CROCODILE) => 2
      case t if (t == SLOTH) => 10
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
      case t if (t == SLOTH) => 0
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
      case t if (t == SLOTH) => 2
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
      case t if (t == SLOTH) => 200
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
      case t if (t == SLOTH) => 200
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
      case t if (t == SLOTH) => 200
      case _ => 0
    }
  }
  
  /** monsters might give item drops when dying */
  def drop(MonsterType: Monster, x: Int, y: Int) {
    MonsterType match {
      case t if (t == RAT) => {new Useable(x, y, ItemType.RATMEAT, false)}
      case _ => {}
    }
  }
  
  /** returns name of the given monster */
  def name(MonsterType: Monster): String = {
    MonsterType match {
      case t if (t == RAT) => "rat"
      case t if (t == BAT) => "bat"
      case t if (t == SNAKE) => "snake"
      case t if (t == SPIDER) => "spider"
      case t if (t == GOBLINA) => "goblin"
      case t if (t == GOBLINB) => "goblin"
      case t if (t == HOUND) => "hound"
      case t if (t == LIZARDA) => "lizard"
      case t if (t == LIZARDB) => "lizard"
      case t if (t == LIZARDC) => "lizard"
      case t if (t == CROCODILE) => "crocodile"
      case t if (t == SLOTH) => "sloth demon"
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
      case t if (t == SLOTH) => "TODO"
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

/** TODO */
object ItemType extends Enumeration with Serializable {

  type Item = Value
  val KNIFE, ROBES, IRONARMOR, STEELSWORD, WOODENSHIELD, RATMEAT, SMALLHEALPOTION, BATTLEAXE, 
  CLOTH1, CLOTH2, IRONSHIELD, KATANA, LARGESHIELD, SHORTSWORD, SMALLSHIELD, STEELARMOR = Value
  
  /** return chances how items occur in game */
  def levelChance(level: Int): Map[Item, Int] = {
    var chances = Map[Item, Int]()
    level match {
      case l if (l == 1) => chances = 
        Map(WOODENSHIELD -> 2, IRONARMOR -> 1, SMALLHEALPOTION -> 4, SHORTSWORD -> 2, 
            SMALLSHIELD -> 1)
      case l if (l == 2) => chances = 
        Map(WOODENSHIELD -> 1, IRONARMOR -> 2, SMALLHEALPOTION -> 4, IRONSHIELD -> 1, 
            SHORTSWORD -> 1, SMALLSHIELD -> 1)
      case l if (l == 3) => chances = 
        Map(WOODENSHIELD -> 1, STEELSWORD -> 1, IRONARMOR -> 1, SMALLHEALPOTION -> 4, 
            BATTLEAXE -> 2, IRONSHIELD -> 1, LARGESHIELD -> 1, SHORTSWORD -> 1, SMALLSHIELD -> 2, 
            STEELARMOR -> 1)
      case l if (l == 4) => chances = 
        Map(STEELSWORD -> 2, IRONARMOR -> 1, SMALLHEALPOTION -> 4, BATTLEAXE -> 1, IRONSHIELD -> 2, 
            LARGESHIELD -> 1, SHORTSWORD -> 1, SMALLSHIELD -> 1, STEELARMOR -> 2)
      case l if (l == 5) => chances = 
        Map(STEELSWORD -> 2, IRONARMOR -> 1, SMALLHEALPOTION -> 4, BATTLEAXE -> 1, IRONSHIELD -> 2, 
            LARGESHIELD -> 1, STEELARMOR -> 2)
      case _ => {chances = Map(KNIFE -> 100)}
    }
    chances
  }
  
  //BATTLEAXE -> , IRONSHIELD -> , LARGESHIELD -> , SHORTSWORD -> , SMALLSHIELD -> , STEELARMOR -> 
  
  /** return chances how items occur in game */
  def shopChance(level: Int): Map[Item, Int] = {
    var chances = levelChance(level)
    level match {
      case l if (l == 1) => chances += (CLOTH1 -> 2, CLOTH2 -> 2)
      case l if (l == 2) => chances += (CLOTH1 -> 2, CLOTH2 -> 1)
      case l if (l == 3) => chances += (CLOTH1 -> 1, CLOTH2 -> 1)
      case l if (l == 4) => chances += (CLOTH1 -> 1)
      case l if (l == 5) => {}
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
      case t if (t == BATTLEAXE) => "weapon"
      case t if (t == CLOTH1) => "armor"
      case t if (t == CLOTH2) => "armor"
      case t if (t == IRONSHIELD) => "shield"
      case t if (t == KATANA) => "weapon"
      case t if (t == LARGESHIELD) => "shield"
      case t if (t == SHORTSWORD) => "weapon"
      case t if (t == SMALLSHIELD) => "shield"
      case t if (t == STEELARMOR) => "armor"
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
      case t if (t == BATTLEAXE) => battleAxeG
      case t if (t == CLOTH1) => cloth1G
      case t if (t == CLOTH2) => cloth2G
      case t if (t == IRONSHIELD) => ironShieldG
      case t if (t == KATANA) => katanaG
      case t if (t == LARGESHIELD) => largeShieldG
      case t if (t == SHORTSWORD) => shortSwordG
      case t if (t == SMALLSHIELD) => smallShieldG
      case t if (t == STEELARMOR) => steelArmorG
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
      case _ => missing
    }
  }

  /** returns armor of the given equipment */
  def armor(ItemType: Item): Double = {
    ItemType match {
      case t if (t == ROBES) => 0
      case t if (t == IRONARMOR) => 1
      case t if (t == WOODENSHIELD) => 0.5
      case t if (t == CLOTH1) => 0
      case t if (t == CLOTH2) => 0
      case t if (t == IRONSHIELD) => 1
      case t if (t == LARGESHIELD) => 1.5
      case t if (t == SMALLSHIELD) => 1
      case t if (t == STEELARMOR) => 1.5
      case _ => 0
    }
  }
  
  /** returns weight of the given equipment */
  def weight(ItemType: Item): Int = {
    ItemType match {
      case t if (t == KNIFE) => 2
      case t if (t == ROBES) => 2
      case t if (t == IRONARMOR) => 25
      case t if (t == STEELSWORD) => 12
      case t if (t == WOODENSHIELD) => 5
      case t if (t == RATMEAT) => 3
      case t if (t == SMALLHEALPOTION) => 2
      case t if (t == BATTLEAXE) => 15
      case t if (t == CLOTH1) => 2
      case t if (t == CLOTH2) => 2
      case t if (t == IRONSHIELD) => 10
      case t if (t == KATANA) => 10
      case t if (t == LARGESHIELD) => 15
      case t if (t == SHORTSWORD) => 8
      case t if (t == SMALLSHIELD) => 5
      case t if (t == STEELARMOR) => 25
      case _ => 0
    }
  }
  
  /** returns block chance of the given equipment */
  def blockChance(ItemType: Item): Int = {
    ItemType match {
      case t if (t == WOODENSHIELD) => 10
      case t if (t == IRONSHIELD) => 15
      case t if (t == LARGESHIELD) => 15
      case t if (t == SMALLSHIELD) => 10
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
      case t if (t == STEELSWORD) => (1, 6, 0)
      case t if (t == BATTLEAXE) => (1, 5, 0)
      case t if (t == KATANA) => (1, 6, 1)
      case t if (t == SHORTSWORD) => (1, 4, 0)
      case _ => (1, 1, 0)
    }
  }
  
  /** returns armor piercing of the given equipment */
  def armorPiercing(ItemType: Item): Int = {
    ItemType match {
      case t if (t == KNIFE) => 0
      case t if (t == STEELSWORD) => 1
      case t if (t == BATTLEAXE) => 2
      case t if (t == KATANA) => 1
      case t if (t == SHORTSWORD) => 1
      case _ => 0
    }
  }
  
  /** returns accuracy of the given equipment */
  def accuracy(ItemType: Item): Int = {
    ItemType match {
      case t if (t == KNIFE) => 100
      case t if (t == STEELSWORD) => 90
      case t if (t == BATTLEAXE) => 80
      case t if (t == KATANA) => 90
      case t if (t == SHORTSWORD) => 90
      case _ => 0
    }
  }
  
  /** returns critical chance of the given equipment */
  def critChance(ItemType: Item): Int = {
    ItemType match {
      case t if (t == KNIFE) => 4
      case t if (t == STEELSWORD) => 5
      case t if (t == BATTLEAXE) => 6
      case t if (t == KATANA) => 5
      case t if (t == SHORTSWORD) => 4
      case _ => 0
    }
  }
  
  /** returns price of the given equipment */
  def price(ItemType: Item): Int = {
    ItemType match {
      case t if (t == KNIFE) => 4
      case t if (t == ROBES) => 2
      case t if (t == IRONARMOR) => 50
      case t if (t == STEELSWORD) => 80
      case t if (t == WOODENSHIELD) => 10
      case t if (t == SMALLHEALPOTION) => 10
      case t if (t == BATTLEAXE) => 50
      case t if (t == CLOTH1) => 5
      case t if (t == CLOTH2) => 5
      case t if (t == IRONSHIELD) => 60
      case t if (t == LARGESHIELD) => 80
      case t if (t == SHORTSWORD) => 20
      case t if (t == SMALLSHIELD) => 20
      case t if (t == STEELARMOR) => 80
      case _ => 0
    }
  }
  
  /** returns name of the given equipment */
  def name(ItemType: Item): String = {
    ItemType match {
      case t if (t == KNIFE) => "knife"
      case t if (t == ROBES) => "robes"
      case t if (t == IRONARMOR) => "iron armor"
      case t if (t == STEELSWORD) => "steel sword"
      case t if (t == WOODENSHIELD) => "wooden shield"
      case t if (t == RATMEAT) => "rat meat"
      case t if (t == SMALLHEALPOTION) => "small healing salve"
      case t if (t == BATTLEAXE) => "battle axe"
      case t if (t == CLOTH1) => "clothes"
      case t if (t == CLOTH2) => "clothes"
      case t if (t == IRONSHIELD) => "iron shield"
      case t if (t == KATANA) => "katana"
      case t if (t == LARGESHIELD) => "large shield"
      case t if (t == SHORTSWORD) => "short sword"
      case t if (t == SMALLSHIELD) => "small shield"
      case t if (t == STEELARMOR) => "steel armor"
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
      case t if (t == BATTLEAXE) => "TODO"
      case t if (t == CLOTH1) => "TODO"
      case t if (t == CLOTH2) => "TODO"
      case t if (t == IRONSHIELD) => "TODO"
      case t if (t == KATANA) => "TODO"
      case t if (t == LARGESHIELD) => "TODO"
      case t if (t == SHORTSWORD) => "TODO"
      case t if (t == SMALLSHIELD) => "TODO"
      case t if (t == STEELARMOR) => "TODO"
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
        addLog(getPlayer.name.toUpperCase.head + getPlayer.name.tail + " eats " + name + ".")

      }
      case i if (i == ItemType.SMALLHEALPOTION) => {
        getPlayer.effectList = getPlayer.effectList :+ new smallHeal(roll(5)+5, getPlayer)
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