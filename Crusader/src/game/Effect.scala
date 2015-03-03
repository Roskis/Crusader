package game

import Main._
import Helpers._

/** Effects like prayers */
object Effect {
  
  val prayerChances = Map("partialRestoration" -> 80, "fullRestoration" -> 20)
  
  /** When player succeeds praying one random prayer is selected */
  def prayer = {
    chooseRandomFromMap(prayerChances) match {
      case p if (p == "partialRestoration") => partialRestoration
      case p if (p == "fullRestoration") => fullRestoration
      case _ => {}
    }
  }

  /** Player regains part of maximum health */
  def partialRestoration = {
    var toAdd = getPlayer.maxHealth * 0.2
    if (getPlayer.health >= 0.8*getPlayer.maxHealth) toAdd = getPlayer.maxHealth - getPlayer.health
    getPlayer.health += toAdd
    addLog("You gained " + toAdd.toInt +  " health.")
  }
  
  /** Player's health is restored to maximum */
  def fullRestoration = {
    var toAdd = getPlayer.maxHealth - getPlayer.health
    getPlayer.health += toAdd
    addLog("You gained " + toAdd.toInt +  " health.")
  }
  
}