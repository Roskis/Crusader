package game

import Main.getRnd
import Math.abs

/** There are four main directions and four intermediate directions */
object Direction extends Enumeration {

  private val rnd = getRnd
  
  type Direction = Value
  val N, NE, E, SE, S, SW, W, NW = Value

  /** Method to get new coordinates when moving to the wanted direction.
   *
   * @param dir is the given direction
   * @param x is the current x coordinate
   * @param y is the current y coordinate
   * @return Tuple2 containing new x and y coordinates
   */
  def getCoordinates(dir: Direction, x: Int, y: Int): Coordinate = {
    dir match {
      case d if (d == N) => new Coordinate(x, y-1)
      case d if (d == E) => new Coordinate(x+1, y)
      case d if (d == S) => new Coordinate(x, y+1)
      case d if (d == W) => new Coordinate(x-1, y)
      case d if (d == NE) => new Coordinate(x+1, y-1)
      case d if (d == NW) => new Coordinate(x-1, y-1)
      case d if (d == SE) => new Coordinate(x+1, y+1)
      case d if (d == SW) => new Coordinate(x-1, y+1)
    }
  }
  
  def getCoordinates(dir: Direction, startCoord: Coordinate): Coordinate = {
    getCoordinates(dir: Direction, startCoord.getX, startCoord.getY)
  }
  def getCoordinates(dir: Direction, tile: Tile): Coordinate = {
    getCoordinates(dir: Direction, tile.getX, tile.getY)
  }
  def getCoordinates(dir: Direction, obj: Object): Coordinate = {
    getCoordinates(dir: Direction, obj.getX, obj.getY)
  }
  
  /** Method to get the direction when looking from start coordinate to end coordinate */
  def getDirection(startCoord: Coordinate, endCoord: Coordinate): Direction = {
    var dx = endCoord.getX - startCoord.getX
    var dy = endCoord.getY - startCoord.getY
    var dir: Direction = null
    if (abs(dx) >= abs(dy)) {
      if (dx >= 0) {
        dir = E
      }
      else {
        dir = W
      }
    }
    else {
      if (dy >= 0) {
        dir = S
      }
      else {
        dir = N
      }
    }
    dir
  }
  
  /** Return random direction. Insert 4 if you want to move only with main directions and 8 if you 
   *  want to allow intermediate directions.
   *
   * @param num number of directions allowed
   * @return Direction random direction
   */
  def randomDirection(num: Int): Direction = {
    rnd.nextInt(num) match {
      case r if (r == 0) => N
      case r if (r == 1) => E
      case r if (r == 2) => S
      case r if (r == 3) => W
      case r if (r == 4) => NW
      case r if (r == 5) => NE
      case r if (r == 6) => SW
      case r if (r == 7) => SE
    }
  }
}