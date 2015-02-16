package game

import collection.mutable.Buffer
import scala.util.Random
import Math.sqrt

class Grid(val size: Int) {
  
  val rnd = new Random
  val map = Array.ofDim[Tile](size, size)
  var stairs: Tile = null
  
  do {
    for (i <- Range(0,size)) {
      for (j <- Range(0,size)) {
        map(i)(j) = new Tile(i, j, 
            if (rnd.nextInt(100) < 10) TileType.WALL 
            else if (i == 0 || j == 0 || i + 1 == size || j + 1 == size) TileType.WALL 
            else TileType.FLOOR)
      }
    }
    
    stairs = new Tile(rnd.nextInt(size-2)+1, rnd.nextInt(size-2)+1, TileType.STAIRS)
    setTile(stairs.getX, stairs.getY, stairs)
  }
  while (!mapIsContinuous)
  
  def neighbors(tile: Tile):Buffer[Tile] = {
    val list = Buffer[Tile]()
    if (isWithinGrid(tile.getX()+1, tile.getY)) list.append(getTile(tile.getX()+1, tile.getY))
    if (isWithinGrid(tile.getX()-1, tile.getY)) list.append(getTile(tile.getX()-1, tile.getY))
    if (isWithinGrid(tile.getX(), tile.getY+1)) list.append(getTile(tile.getX(), tile.getY+1))
    if (isWithinGrid(tile.getX(), tile.getY-1)) list.append(getTile(tile.getX(), tile.getY-1))
    list
  }
  
  def randomX() = rnd.nextInt(size-2)+1
  def randomY() = randomX
  
  def isWithinGrid(x: Int, y: Int): Boolean = x < size - 1 && x > 0 && y < size - 1 && y > 0
  
  def setTile(x: Int, y: Int, tile: Tile) = map(x)(y) = tile
  def getTile(x: Int, y: Int) = if (x < size && y < size) map(x)(y) else map(1)(1)
  
  def draw() {
    for (i <- Range(0,size)) {
      for (j <- Range(0,size)) {
        if (getTile(i, j).xDif(Main.player) <= 16 && getTile(i, j).yDif(Main.player) <= 8) 
          map(i)(j).draw
      }
    }
  }
  
  def distance(tile1: Tile, tile2: Tile): Int = {
    tile1.distance(tile2)
  }
  
  def giveRandomNonBlockinCoordinates(): Tuple2[Int, Int] = {
    val tile = giveRandomNonBlockingTile 
    (tile.getX, tile.getY)
  }
  
  def giveRandomNonBlockingTile(): Tile = {
    var tile: Tile = null
    do {tile = getTile(randomX, randomY)}
    while (tile.blockMovement)
    tile
  }
  
  def mapIsContinuous(): Boolean = {
    var checkList = Map[Int, Tile]()
    var patternNumber: Int = 0
    
    for (i <- Range(0,size)) {
      for (j <- Range(0,size)) {
        if (!map(i)(j).blockMovement) {
          map(i)(j).label = size*size+1
          for (near <- neighbors(map(i)(j))) {
            if (near.label != 0 && near.label < map(i)(j).label) map(i)(j).label = near.label
            else if (near.label != 0 && near.label > map(i)(j).label) changeLabels(near.label, map(i)(j).label)
          }
          if (map(i)(j).label == size*size+1) {
            patternNumber += 1
            map(i)(j).label = patternNumber
            checkList += ((patternNumber, map(i)(j)))
          }
        }
      }
    }
  
  for (item <- checkList) changeLabels(item._1, areaHasSmallerNeighbor(item._1))
  
  def areaHasSmallerNeighbor(num: Int):Int = {
    var num2 = num
    for (n <- Range(0,size)) {
      for (m <- Range(0,size)) {
        if (map(n)(m).label == num) {
          for (near <- neighbors(map(n)(m))) 
            if (near.label != 0 && near.label < map(n)(m).label) num2 = near.label
        }
      }
    }
    num2
  }
  
  def changeLabels(from: Int, to: Int) {
    for (i <- Range(0,size)) {
      for (j <- Range(0,size)) {
        if (map(i)(j).label == from) map(i)(j).label = to
      }
    }
    checkList -= from
  }
  
  var boolean = true
  for (i <- Range(0,size)) {
    for (j <- Range(0,size)) {
      if (map(i)(j).label > 1) boolean = false
    }
  }
  boolean
  }
  
}