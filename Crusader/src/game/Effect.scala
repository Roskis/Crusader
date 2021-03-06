package game

import scala.collection.mutable.Buffer

import scala.xml.XML

import Main._
import Helpers._
import Direction._

/** Player used prayers */
object Prayers extends Enumeration {
  
  type Prayer = Value
  val PARTIALRESTORATION, FULLRESTORATION, LIGHTNINGBOLT, GOLDLOSS, EXPERIENCELOSS, DEMENTIA, 
    STAIRS, SMITE, GOLDGAIN, EXPERIENCEGAIN, CLAIRVOYANCE, ITEM, FEAR, AOEDAMAGE, BLINDINGLIGHT, 
    REVEALSECRET, IMMUNITY, BUFF, VISION, LEVELUP, LEVELDOWN, TIMESTOP, TEMPBOOST = Value
  val rnd = getRnd
  
  /** Player regains part of maximum health */
  def partialRestoration = {
    var toAdd = getPlayer.maxHealth * 0.2
    if (getPlayer.health >= 0.8*getPlayer.maxHealth) toAdd = getPlayer.maxHealth - getPlayer.health
    getPlayer.health += toAdd
    addLog("You gained " + toAdd.toInt +  " health.")
  }
  
  /** Player's health is restored to maximum */
  def fullRestoration = {
    val toAdd = getPlayer.maxHealth - getPlayer.health
    getPlayer.health += toAdd
    addLog("You gained " + toAdd.toInt +  " health.")
  }
  
  /** Player is fired with lightning bolt */
  def lightningBolt = {
    var toRemove = 0
    if (getPlayer.health != 0) toRemove = rnd.nextInt(getPlayer.maxHealth.toInt)+1
    getPlayer.health -= toRemove
    addLog("You lost " + toRemove.toInt +  " health.")
  }
  
  /** Player loses part of gold */
  def goldLoss = {
    var toRemove = 0
    if (getPlayer.gold != 0) toRemove = rnd.nextInt(getPlayer.gold.toInt)+1
    getPlayer.gold -= toRemove
    addLog("You lost " + toRemove.toInt +  " gold.")
  }
  
  /** Player loses part of experience */
  def experienceLoss = {
    var toRemove = 0
    if (getPlayer.experience != 0) toRemove = rnd.nextInt(getPlayer.experience.toInt)+1
    getPlayer.experience -= toRemove
    addLog("You lost " + toRemove.toInt +  " experience.")
  }
  
  /** Player experiences memory loss */
  def dementia = {
    getGrid.unexploreAll
    addLog("You suffer from dementia.")
  }
  
  /** Stairs are moved to the player's location */
  def stairs = {
    if (getLevel%5 != 0) {
      getGrid.moveStairsToPlayer
      addLog("Stairs appear near you.")
    }
  }
  
  /** Smite the monster player is fighting with */
  def smite = {
    if (getLastMonster != null) {
      val mon = getLastMonster
      val damage = roll((getPlayer.getZeal + 1).toInt, 3)
      mon.health -= damage
      if (mon.health <= 0) mon.kill
      addLog(mon.name.toUpperCase.head + mon.name.tail + " takes " + damage.toInt +  " damage.")
    }
  }
  
  /** Give player some gold */
  def goldGain = {
    val toAdd = roll(getPlayer.getDiligence.toInt, 10) + rnd.nextInt(10) + 1
    getPlayer.gold += toAdd
    addLog("You gain " + toAdd.toInt +  " gold.")
  }
  
  /** Give player some experience */
  def experienceGain = {
    val toAdd = roll(getPlayer.getDiligence.toInt, (getPlayer.totalLevel/4).toInt) + rnd.nextInt(10) + 1
    getPlayer.experience += toAdd
    addLog("You gain " + toAdd.toInt + " experience.")
  }
  
  /** Reveal whole map */
  def clairvoyance = {
    getGrid.exploreAll
    addLog("Whole map is revealed to you.")
  }
  
  /** Give player item */
  def item = {
    if (rnd.nextBoolean) new Equipment(getPlayer.getX, getPlayer.getY, ItemType.KATANA, false)
    else new Equipment(getPlayer.getX, getPlayer.getY, ItemType.MAGICSWORD, false)
    addLog("Weapon appears under your feet.")
  }
  
  /** Casts fear to nearby enemies */
  def fear = {
    for (monster <- getMonsterList) if (monster.distance(getPlayer) < 5) 
      monster.effectList = monster.effectList :+ new fear(roll(3)+3, monster, getPlayer)
    addLog("Divine light surrounds you scaring nearby monsters for a while.")
  }
  
  /** Damages nearby enemies */
  def aoeDamage = {
    var monstersToDamage = Buffer[Monster]()
    for (monster <- getMonsterList) if (monster.distance(getPlayer) < 5) monstersToDamage.append(monster)
    for (monster <- monstersToDamage) {
      monster.health -= roll(getPlayer.getZeal.toInt + 1, 3)
      if (monster.health <= 0) monster.kill
    }
    addLog("Burning light surrounds you damaging nearby monsters.")
  }
  
  /** Blind nearby enemies */
  def blindingLight = {
    for (monster <- getMonsterList) if (monster.distance(getPlayer) < 5) 
      monster.effectList = monster.effectList :+ new blind(roll(3)+3, monster, getPlayer)
    addLog("Bright light surrounds you blinding nearby monsters for a while.")
  }
  
  /** Reveal location of secret */
  def revealSecret = {
    if (getGrid.secretTile != null) addLog(
        "You hear wishpers about long forgotten hidden place in coordinates of X: " + 
        getGrid.secretTile.getX + " and Y: " + getGrid.secretTile.getY + ".")
  }
  
  /** Immunity from enemy attacks */
  def immunity = {
    getPlayer.effectList = getPlayer.effectList :+ new immunity(roll(3)+3, getPlayer)
    addLog("Mystical forces protect you for a short duration.")
  }
  
  /** General buff */
  def buff = {
    getPlayer.effectList = getPlayer.effectList :+ new buff(roll(5)+5, getPlayer)
    addLog("You feel great temporary increase in your physical abilities.")
  }
  
  /** Increased range of vision */
  def vision = {
    getPlayer.effectList = getPlayer.effectList :+ new vision(roll(10)+10, getPlayer)
    addLog("Your eyes glow heavenly light allowing you to see farther for a while.")
  }

  /** Level up a random skill */
  def levelUp = {
    val text = "Your " + 
    (rnd.nextInt(7) match {
      case n if (n == 0) => {
        getPlayer.zeal += 1
        "Zeal"
      }
      case n if (n == 1) => {
        getPlayer.humility += 1
        "Humility"
        }
      case n if (n == 2) => {
        getPlayer.temperance += 1
        "Temperance"
        }
      case n if (n == 3) => {
        getPlayer.kindness += 1
        "Kindness"
        }
      case n if (n == 4) => {
        getPlayer.patience += 1
        "Patience"
        }
      case n if (n == 5) => {
        getPlayer.charity += 1
        "Charity"
        }
      case n if (n == 6) => {
        getPlayer.diligence += 1
        "Diligence"
        }
    }) + " is permanently increased by one."
    addLog(text)
  }
  
  /** Lose a random skill */
  def levelDown = {
    if (getPlayer.totalLevel <= 0) {}
    else {
      val list = List(getPlayer.getZeal, getPlayer.getHumility, getPlayer.getTemperance, getPlayer.getKindness, 
          getPlayer.getPatience, getPlayer.getCharity, getPlayer.getDiligence)
      var skillNum = 0
      do {skillNum = rnd.nextInt(list.size)}
      while (list(skillNum) == 0)
      val text = "Your " + (skillNum match {
        case 0 => {
          getPlayer.zeal -= 1
          "Zeal"
          }
        case 1 => {
          getPlayer.humility -= 1
          "Humility"
        }
        case 2 => {
          getPlayer.temperance -= 1
          "Temperance"
        }
        case 3 => {
          getPlayer.kindness -= 1
          "Kindness"
        }
        case 4 => {
          getPlayer.patience -= 1
          "Patience"
        }
        case 5 => {
          getPlayer.charity -= 1
          "Charity"
        }
        case 6 => {
          getPlayer.diligence -= 1
          "Diligence"
        }
        case _ => {"skill name not found"}
      }) + " is permanently decreased by one."
      addLog(text)
    }
  }
  
  /** Timestop prayer */
  def timeStop = {
    getPlayer.effectList = getPlayer.effectList :+ new timestop(roll(10)+10, getPlayer)
    addLog("Time around you is temporarily frozen.")
  }
  
  /** Temporary level boost */
  def tempBoost = {
    val text = "Your " + 
    (rnd.nextInt(7) match {
      case n if (n == 0) => {
        getPlayer.effectList = getPlayer.effectList :+ new tempZeal(roll(10)+10, getPlayer)
        "Zeal"
      }
      case n if (n == 1) => {
        getPlayer.effectList = getPlayer.effectList :+ new tempHumility(roll(10)+10, getPlayer)
        "Humility"
        }
      case n if (n == 2) => {
        getPlayer.effectList = getPlayer.effectList :+ new tempTemperance(roll(10)+10, getPlayer)
        "Temperance"
        }
      case n if (n == 3) => {
        getPlayer.effectList = getPlayer.effectList :+ new tempKindness(roll(10)+10, getPlayer)
        "Kindness"
        }
      case n if (n == 4) => {
        getPlayer.effectList = getPlayer.effectList :+ new tempPatience(roll(10)+10, getPlayer)
        "Patience"
        }
      case n if (n == 5) => {
        getPlayer.effectList = getPlayer.effectList :+ new tempCharity(roll(10)+10, getPlayer)
        "Charity"
        }
      case n if (n == 6) => {
        getPlayer.effectList = getPlayer.effectList :+ new tempDiligence(roll(10)+10, getPlayer)
        "Diligence"
        }
    }) + " is temporarily increased by one."
    addLog(text)
  }
  
}

/** TODO */
object EffectType extends Enumeration with Serializable {

  type Effect = Value
  val SMALLHEAL, POISON, BIND, FEAR, BLIND, IMMUNITY, BUFF, VISION, TIMESTOP, TEMPCHARITY, 
  TEMPDILIGENCE, TEMPHUMILITY, TEMPKINDNESS, TEMPPATIENCE, TEMPTEMPERANCE, TEMPZEAL = Value

  private var smallHeal: scala.xml.Node = null
  private var poison: scala.xml.Node = null
  private var bind: scala.xml.Node = null
  private var fear: scala.xml.Node = null
  private var blind: scala.xml.Node = null
  private var immunity: scala.xml.Node = null
  private var buff: scala.xml.Node = null
  private var vision: scala.xml.Node = null
  private var timestop: scala.xml.Node = null
  private var tempCharity: scala.xml.Node = null
  private var tempDiligence: scala.xml.Node = null
  private var tempHumility: scala.xml.Node = null
  private var tempKindness: scala.xml.Node = null
  private var tempPatience: scala.xml.Node = null
  private var tempTemperance: scala.xml.Node = null
  private var tempZeal: scala.xml.Node = null
  
  private val xml = XML.loadFile("data/effects.xml")
  
  for (i <- xml.child) i match {
    case o if ((o \ "EffectType").text == "SMALLHEAL") => smallHeal = o
    case o if ((o \ "EffectType").text == "POISON") => poison = o
    case o if ((o \ "EffectType").text == "BIND") => bind = o
    case o if ((o \ "EffectType").text == "FEAR") => fear = o
    case o if ((o \ "EffectType").text == "BLIND") => blind = o
    case o if ((o \ "EffectType").text == "IMMUNITY") => immunity = o
    case o if ((o \ "EffectType").text == "BUFF") => buff = o
    case o if ((o \ "EffectType").text == "VISION") => vision = o
    case o if ((o \ "EffectType").text == "TIMESTOP") => timestop = o
    case o if ((o \ "EffectType").text == "TEMPCHARITY") => tempCharity = o
    case o if ((o \ "EffectType").text == "TEMPDILIGENCE") => tempDiligence = o
    case o if ((o \ "EffectType").text == "TEMPHUMILITY") => tempHumility = o
    case o if ((o \ "EffectType").text == "TEMPKINDNESS") => tempKindness = o
    case o if ((o \ "EffectType").text == "TEMPPATIENCE") => tempPatience = o
    case o if ((o \ "EffectType").text == "TEMPTEMPERANCE") => tempTemperance = o
    case o if ((o \ "EffectType").text == "TEMPZEAL") => tempZeal = o
    case _ => {}
  }
  
  def getXml(EffectType: Effect): scala.xml.Node = {
    EffectType match {
      case t if (t == SMALLHEAL) => smallHeal
      case t if (t == POISON) => poison
      case t if (t == BIND) => bind
      case t if (t == FEAR) => fear
      case t if (t == BLIND) => blind
      case t if (t == IMMUNITY) => immunity
      case t if (t == BUFF) => buff
      case t if (t == VISION) => vision
      case t if (t == TIMESTOP) => timestop
      case t if (t == TEMPCHARITY) => tempCharity
      case t if (t == TEMPDILIGENCE) => tempDiligence
      case t if (t == TEMPHUMILITY) => tempHumility
      case t if (t == TEMPKINDNESS) => tempKindness
      case t if (t == TEMPPATIENCE) => tempPatience
      case t if (t == TEMPTEMPERANCE) => tempTemperance
      case t if (t == TEMPZEAL) => tempZeal
      case _ => null
    }
  }
  
  def name(EffectType: Effect): String = {
    if (getXml(EffectType) != null && (getXml(EffectType) \ "name").text != "") 
      (getXml(EffectType) \ "name").text
    else "Unknown effect name"
  }  
  
  def description(EffectType: Effect): String = {
    if (getXml(EffectType) != null && (getXml(EffectType) \ "description").text != "") 
      (getXml(EffectType) \ "description").text
    else "Unknown effect name"
  }
  
  def armor(EffectType: Effect): Double = {
    if (getXml(EffectType) != null && (getXml(EffectType) \ "armor").text != "") 
      (getXml(EffectType) \ "armor").text.toDouble
    else 0.0
  }
  
  def shieldArmor(EffectType: Effect): Double = {
    if (getXml(EffectType) != null && (getXml(EffectType) \ "shieldArmor").text != "") 
      (getXml(EffectType) \ "shieldArmor").text.toDouble
    else 0.0
  }
  
  def weight(EffectType: Effect): Double = {
    if (getXml(EffectType) != null && (getXml(EffectType) \ "weight").text != "") 
      (getXml(EffectType) \ "weight").text.toDouble
    else 0
  }
  
  def blockChance(EffectType: Effect): Double = {
    if (getXml(EffectType) != null && (getXml(EffectType) \ "blockChance").text != "") 
      (getXml(EffectType) \ "blockChance").text.toDouble
    else 0
  }

  def damage(EffectType: Effect): Tuple3[Int, Int, Int] = {
    if (getXml(EffectType) != null && (getXml(EffectType) \ "damage").text != "") {
      val a = (getXml(EffectType) \ "damage").text.split(",")
      (a(0).toInt, a(1).toInt, a(2).toInt)
    }
    else (0, 0, 0)
  }
  
  def armorPiercing(EffectType: Effect): Double = {
    if (getXml(EffectType) != null && (getXml(EffectType) \ "armorPiercing").text != "") 
      (getXml(EffectType) \ "armorPiercing").text.toDouble
    else 0
  }
  
  def accuracy(EffectType: Effect): Double = {
    if (getXml(EffectType) != null && (getXml(EffectType) \ "accuracy").text != "") 
      (getXml(EffectType) \ "accuracy").text.toDouble
    else 0
  }
  
  def critChance(EffectType: Effect): Double = {
    if (getXml(EffectType) != null && (getXml(EffectType) \ "critChance").text != "") 
      (getXml(EffectType) \ "critChance").text.toDouble
    else 0
  }
  
  def zeal(EffectType: Effect): Double = {
    if (getXml(EffectType) != null && (getXml(EffectType) \ "zeal").text != "") 
      (getXml(EffectType) \ "zeal").text.toDouble
    else 0.0
  }
  
  def humility(EffectType: Effect): Double = {
    if (getXml(EffectType) != null && (getXml(EffectType) \ "humility").text != "") 
      (getXml(EffectType) \ "humility").text.toDouble
    else 0.0
  }
  
  def temperance(EffectType: Effect): Double = {
    if (getXml(EffectType) != null && (getXml(EffectType) \ "temperance").text != "") 
      (getXml(EffectType) \ "temperance").text.toDouble
    else 0.0
  }
  
  def kindness(EffectType: Effect): Double = {
    if (getXml(EffectType) != null && (getXml(EffectType) \ "kindness").text != "") 
      (getXml(EffectType) \ "kindness").text.toDouble
    else 0.0
  }
  
  def patience(EffectType: Effect): Double = {
    if (getXml(EffectType) != null && (getXml(EffectType) \ "patience").text != "") 
      (getXml(EffectType) \ "patience").text.toDouble
    else 0.0
  }
  
  def charity(EffectType: Effect): Double = {
    if (getXml(EffectType) != null && (getXml(EffectType) \ "charity").text != "") 
      (getXml(EffectType) \ "charity").text.toDouble
    else 0.0
  }
  
  def diligence(EffectType: Effect): Double = {
    if (getXml(EffectType) != null && (getXml(EffectType) \ "diligence").text != "") 
      (getXml(EffectType) \ "diligence").text.toDouble
    else 0.0
  }
  
  def health(EffectType: Effect): Double = {
    if (getXml(EffectType) != null && (getXml(EffectType) \ "health").text != "") 
      (getXml(EffectType) \ "health").text.toDouble
    else 0.0
  }
  
  def viewRange(EffectType: Effect): Double = {
    if (getXml(EffectType) != null && (getXml(EffectType) \ "viewRange").text != "") 
      (getXml(EffectType) \ "viewRange").text.toDouble
    else 0.0
  }
  
  def prayChance(EffectType: Effect): Double = {
    if (getXml(EffectType) != null && (getXml(EffectType) \ "prayChance").text != "") 
      (getXml(EffectType) \ "prayChance").text.toDouble
    else 0.0
  }
  
  def xpModifier(EffectType: Effect): Double = {
    if (getXml(EffectType) != null && (getXml(EffectType) \ "xpModifier").text != "") 
      (getXml(EffectType) \ "xpModifier").text.toDouble
    else 1.0
  }
  
  def goldModifier(EffectType: Effect): Double = {
    if (getXml(EffectType) != null && (getXml(EffectType) \ "goldModifier").text != "") 
      (getXml(EffectType) \ "goldModifier").text.toDouble
    else 1.0
  }
  
  def pietyModifier(EffectType: Effect): Double = {
    if (getXml(EffectType) != null && (getXml(EffectType) \ "pietyModifier").text != "") 
      (getXml(EffectType) \ "pietyModifier").text.toDouble
    else 1.0
  }
  
  def dodge(EffectType: Effect): Double = {
    if (getXml(EffectType) != null && (getXml(EffectType) \ "dodge").text != "") 
      (getXml(EffectType) \ "dodge").text.toDouble
    else 0.0
  }
  
}

/** Temporary effects on characters */
trait Effect extends Serializable {
  
  val effectType: EffectType.Value
  val target: Character
  var duration: Int
  def caster: Character
  def turn: Unit = {
    duration -= 1
    if (duration == 0) addLog("Effect of " + name + " has ended.")
  }
  
  def name = EffectType.name(effectType)
  def description = EffectType.description(effectType)
  def armor = EffectType.armor(effectType)
  def shieldArmor = EffectType.shieldArmor(effectType)
  def weight: Double = EffectType.weight(effectType)
  def blockChance: Double = EffectType.blockChance(effectType)
  def armorPiercing: Double = EffectType.armorPiercing(effectType)
  def accuracy: Double = EffectType.accuracy(effectType)
  def critChance: Double = EffectType.critChance(effectType)
  def zeal: Double = EffectType.zeal(effectType)
  def humility: Double = EffectType.humility(effectType)
  def temperance: Double = EffectType.temperance(effectType)
  def kindness: Double = EffectType.kindness(effectType)
  def patience: Double = EffectType.patience(effectType)
  def charity: Double = EffectType.charity(effectType)
  def diligence: Double = EffectType.diligence(effectType)
  def health: Double = EffectType.health(effectType)
  def viewRange: Double = EffectType.viewRange(effectType)
  def prayChance: Double = EffectType.prayChance(effectType)
  def xpModifier: Double = EffectType.xpModifier(effectType)
  def goldModifier: Double = EffectType.goldModifier(effectType)
  def pietyModifier: Double = EffectType.pietyModifier(effectType)
  def dodge: Double = EffectType.dodge(effectType)
  def damage: Double = roll(EffectType.damage(effectType)._1, EffectType.damage(effectType)._2) + EffectType.damage(effectType)._3
  
}

/** Small heal heal over time */
class smallHeal(dur: Int, tar: Character) extends Effect with Serializable {
  val effectType = EffectType.SMALLHEAL
  val target = tar
  var duration = dur
  def caster = null
  override def turn {
    val toAdd = roll(2)
    target.health += toAdd
    target match {
      case m: Monster => {if (m.health > MonsterType.maxHP(m.mType)) m.health = MonsterType.maxHP(m.mType)}
      case p: Player => {if (p.health > p.maxHealth) p.health = p.maxHealth}
      case _ => {}
    }
    duration -= 1
    addLog(target.name.toUpperCase.head + target.name.tail + " gains " + toAdd + " health from " + name + ".")
    if (duration == 0) addLog("Effect of " + name + " has ended.")
  }
}

/** Poison hurts over time */
class poison(dur: Int, tar: Character, cas: Character) extends Effect with Serializable {
  val effectType = EffectType.POISON
  val target = tar
  var duration = dur
  def caster = cas
  override def turn {
    val toRemove = roll(2)
    if (roll(3) == 1) {
      target.health -= toRemove
      addLog(target.name.toUpperCase.head + target.name.tail + " loses " + toRemove + " health from " + name + ".")
    }
    duration -= 1
    if (duration == 0) addLog("Effect of " + name + " has ended.")
  }
}

/** Bind entangles its target */
class bind(dur: Int, tar: Character, cas: Character) extends Effect with Serializable {
  val effectType = EffectType.BIND
  val target = tar
  var duration = dur
  def caster = cas
}

/** Fear spell's effect */
class fear(dur: Int, tar: Character, cas: Character) extends Effect with Serializable {
  val effectType = EffectType.FEAR
  val target = tar
  var duration = dur
  def caster = cas
  override def turn {
    tar.move(getCoordinates(getDirection(caster.getCoordinate, tar.getCoordinate), tar))
    duration -= 1
    if (duration == 0) addLog("Effect of " + name + " has ended.")
  }
}

/** Miss spell's effect */
class blind(dur: Int, tar: Character, cas: Character) extends Effect with Serializable {
  val effectType = EffectType.BLIND
  val target = tar
  var duration = dur
  def caster = cas
}

/** Immunity from attacks */
class immunity(dur: Int, tar: Character) extends Effect with Serializable {
  val effectType = EffectType.IMMUNITY
  val target = tar
  var duration = dur
  def caster = null
}

/** General buff */
class buff(dur: Int, tar: Character) extends Effect with Serializable {
  val effectType = EffectType.BUFF
  val target = tar
  var duration = dur
  def caster = null
}

/** Increased range of vision */
class vision(dur: Int, tar: Character) extends Effect with Serializable {
  val effectType = EffectType.VISION
  val target = tar
  var duration = dur
  def caster = null
}

/** Timestop for player */
class timestop(dur: Int, tar: Character) extends Effect with Serializable {
  val effectType = EffectType.TIMESTOP
  val target = tar
  var duration = dur
  def caster = null
}

/** Temporary charity boost */
class tempCharity(dur: Int, tar: Character) extends Effect with Serializable {
  val effectType = EffectType.TEMPCHARITY
  val target = tar
  var duration = dur
  def caster = null
}

/** Temporary diligene boost */
class tempDiligence(dur: Int, tar: Character) extends Effect with Serializable {
  val effectType = EffectType.TEMPDILIGENCE
  val target = tar
  var duration = dur
  def caster = null
}

/** Temporary humility boost */
class tempHumility(dur: Int, tar: Character) extends Effect with Serializable {
  val effectType = EffectType.TEMPHUMILITY
  val target = tar
  var duration = dur
  def caster = null
}

/** Temporary kindness boost */
class tempKindness(dur: Int, tar: Character) extends Effect with Serializable {
  val effectType = EffectType.TEMPKINDNESS
  val target = tar
  var duration = dur
  def caster = null
}

/** Temporary patience boost */
class tempPatience(dur: Int, tar: Character) extends Effect with Serializable {
  val effectType = EffectType.TEMPPATIENCE
  val target = tar
  var duration = dur
  def caster = null
}

/** Temporary temperance boost */
class tempTemperance(dur: Int, tar: Character) extends Effect with Serializable {
  val effectType = EffectType.TEMPTEMPERANCE
  val target = tar
  var duration = dur
  def caster = null
}

/** Temporary zeal boost */
class tempZeal(dur: Int, tar: Character) extends Effect with Serializable {
  val effectType = EffectType.TEMPZEAL
  val target = tar
  var duration = dur
  def caster = null
}
