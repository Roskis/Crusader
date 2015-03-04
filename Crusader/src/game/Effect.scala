package game

import Main._
import Helpers._

/** Effects like prayers */
object Effect extends Enumeration {
  
  type Prayer = Value
  val PARTIALRESTORATION, FULLRESTORATION = Value
  
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