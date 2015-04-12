package game

import scala.collection.mutable.Buffer

import Main._
import Helpers._
import Direction._

/** Player used prayers */
object Prayers extends Enumeration {
  
  type Prayer = Value
  val PARTIALRESTORATION, FULLRESTORATION, LIGHTNINGBOLT, GOLDLOSS, EXPERIENCELOSS, DEMENTIA, 
    STAIRS, SMITE, GOLDGAIN, EXPERIENCEGAIN, CLAIRVOYANCE, ITEM, FEAR, AOEDAMAGE, BLINDINGLIGHT, 
    REVEALSECRET, IMMUNITY = Value
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
    if (getPlayer.health != 0) toRemove = rnd.nextInt(getPlayer.maxHealth)+1
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
    if (getGrid.isWithinGrid(getGrid.getStairs.getX, getGrid.getStairs.getY)) {
      getGrid.moveStairsToPlayer
      addLog("Stairs appear near you.")
    }
  }
  
  /** Smite the monster player is fighting with */
  def smite = {
    if (getLastMonster != null) {
      val mon = getLastMonster
      val damage = roll(getPlayer.zeal + 1, 3)
      mon.health -= damage
      if (mon.health <= 0) mon.kill
      addLog(mon.name.toUpperCase.head + mon.name.tail + " takes " + damage.toInt +  " damage.")
    }
  }
  
  /** Give player some gold */
  def goldGain = {
    val toAdd = roll(getPlayer.diligence, 10) + rnd.nextInt(10) + 1
    getPlayer.gold += toAdd
    addLog("You gain " + toAdd.toInt +  " gold.")
  }
  
  /** Give player some experience */
  def experienceGain = {
    val toAdd = roll(getPlayer.diligence, (getPlayer.totalLevel/4).toInt) + rnd.nextInt(10) + 1
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
    new Equipment(getPlayer.getX, getPlayer.getY, ItemType.KATANA, false)
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
      monster.health -= roll(getPlayer.zeal + 1, 3)
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
  
}

/** Temporary effects on characters */
trait Effect extends Serializable {
  val name: String
  val target: Character
  var duration: Int
  def caster: Character
  def turn: Unit
}

/** Small heal heal over time */
class smallHeal(dur: Int, tar: Character) extends Effect with Serializable {
  val name = "healing salve"
  val target = tar
  var duration = dur
  def caster = null
  def turn {
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
  val name = "poison"
  val target = tar
  var duration = dur
  def caster = cas
  def turn {
    val toRemove = roll(2)-1
    if (toRemove != 0) {
      target.health -= toRemove
      addLog(target.name.toUpperCase.head + target.name.tail + " loses " + toRemove + " health from " + name + ".")
    }
    duration -= 1
    if (duration == 0) addLog("Effect of " + name + " has ended.")
  }
}

/** Bind entangles its target */
class bind(dur: Int, tar: Character, cas: Character) extends Effect with Serializable {
  val name = "Bind"
  val target = tar
  var duration = dur
  def caster = cas
  def turn {
    duration -= 1
    if (duration == 0) addLog("Effect of " + name + " has ended.")
  }
}

/** Fear spell's effect */
class fear(dur: Int, tar: Character, cas: Character) extends Effect with Serializable {
  val name = "Fear"
  val target = tar
  var duration = dur
  def caster = cas
  def turn {
    tar.move(getCoordinates(getDirection(caster.getCoordinate, tar.getCoordinate), tar))
    duration -= 1
    if (duration == 0) addLog("Effect of " + name + " has ended.")
  }
}

/** Miss spell's effect */
class blind(dur: Int, tar: Character, cas: Character) extends Effect with Serializable {
  val name = "Miss"
  val target = tar
  var duration = dur
  def caster = cas
  def turn {
    duration -= 1
    if (duration == 0) addLog("Effect of " + name + " has ended.")
  }
}

/** Immunity from attacks */
class immunity(dur: Int, tar: Character) extends Effect with Serializable {
  val name = "Immunity"
  val target = tar
  var duration = dur
  def caster = null
  def turn = {
    duration -= 1
    if (duration == 0) addLog("Effect of " + name + " has ended.")
  }
}