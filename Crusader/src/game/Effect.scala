package game

import Main._
import Helpers._

/** Player used prayers */
object Prayers extends Enumeration {
  
  type Prayer = Value
  val PARTIALRESTORATION, FULLRESTORATION, LIGHTNINGBOLT, GOLDLOSS, EXPERIENCELOSS, DEMENTIA, 
    STAIRS, SMITE, GOLDGAIN, EXPERIENCEGAIN, CLAIRVOYANCE = Value
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
    getGrid.moveStairsToPlayer
    addLog("Stairs appear near you.")
  }
  
  /** Smite the monster player is fighting with */
  def smite = {
    if (getLastMonster != null) {
      val damage = roll(getPlayer.zeal, 6)
      getLastMonster.health -= damage
      if (getLastMonster.health <= 0) getLastMonster.kill
      addLog(getLastMonster.name.toUpperCase.head + getLastMonster.name.tail + " takes " + damage.toInt +  " damage.")
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
  
  /** reveal whole map */
  def clairvoyance = {
    getGrid.exploreAll
    addLog("Whole map is revealed to you.")
  }
}

/** Temporary effects on characters */
trait Effect {
  val name: String
  val target: Character
  var duration: Int
  def turn: Unit
}

/** Small heal heal over time */
class smallHeal(dur: Int, tar: Character) extends Effect {
  val name = "healing salve"
  val target = tar
  var duration = dur
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
class poison(dur: Int, tar: Character) extends Effect {
  val name = "poison"
  val target = tar
  var duration = dur
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