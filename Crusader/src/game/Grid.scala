package game

import collection.mutable.Buffer
import Math.sqrt
import Main._

/** Grid is reponsible for handling the map */
class Grid() {
  
  private val rnd = getRnd
  private var size: Int = 100
  private var map = Array.ofDim[Tile](size, size)
  var stairs: Tile = new Tile(-100, -100, TileType.STAIRS)
  
  /** Different levels make different maps */
  if (getLevel < 5) {
    size = rnd.nextInt(5)+40
    map = Array.ofDim[Tile](size, size)
    /** We will redo the map while it's not continuous */
    do {
      makeMap1(45, 4, 4)
    }
    while (!mapIsContinuous)
  }
  else if (getLevel == 5) {
    size = 21
    map = Array.ofDim[Tile](size, size)
    makeBoss1
  }
  else {
    size = 10
    map = Array.ofDim[Tile](size, size)
    testMap
  }

  /** Temporary bossmap */
  def makeBoss1() {
    map(size/2)(size/2) = new Tile(size/2, size/2, TileType.FLOOR)
    for (i <- Range(0,size)) {
      for (j <- Range(0,size)) {
        map(i)(j) = new Tile(i, j, TileType.FLOOR)
        map(i)(j) = new Tile(i, j, if (map(i)(j).distance(map(size/2)(size/2)) <= 9) TileType.FLOOR
        else TileType.WALL)
      }
    }
  getPlayer.setX(size/8)
  getPlayer.setY(size/2)
  }
  
  /** Make first episode map.
   *
   * @param startProcent chances the starting floor area
   * @param birthlimit and deathlimit change how cellular automata work for map
   */
  def makeMap1(startProcent: Int, birthlimit: Int, deathlimit: Int) = {
    for (i <- Range(0,size)) {
      for (j <- Range(0,size)) {
        map(i)(j) = new Tile(i, j, 
            if (rnd.nextInt(100) < startProcent) TileType.WALL 
            else if (i == 0 || j == 0 || i + 1 == size || j + 1 == size) TileType.WALL 
            else TileType.FLOOR)
      }
    }
    roundEdges(birthlimit, deathlimit)
    roundEdges(birthlimit, deathlimit)
    roundEdges(birthlimit, deathlimit)
    
    var playerPosition = giveRandomNonBlockinCoordinates
    do {
      addStairs
      playerPosition = giveRandomNonBlockinCoordinates
      getPlayer.setX(playerPosition._1)
      getPlayer.setY(playerPosition._2)
    }
    while (getPlayer.distance(getStairs) < 15)
    
  }
  
  /** cellular automata function to round map a bit */
  def roundEdges(birthlimit: Int, deathlimit: Int) = {
    var nbs = 0
    for (i <- Range(0,size)) {
      for (j <- Range(0,size)) {
        nbs = countAliveNeighbours(map(i)(j))
        if (getTile(i,j).getType == TileType.FLOOR) {
          if (nbs < deathlimit) map(i)(j).label = 0
          else map(i)(j).label = 1
        }
        else {
          if (nbs > birthlimit) map(i)(j).label = 1
          else map(i)(j).label = 0
        }
        
      }
    }
    
    for (i <- Range(0,size)) {
      for (j <- Range(0,size)) {
        if (map(i)(j).label == 1) setTile(i, j, new Tile(i, j, TileType.FLOOR))
        else setTile(i,j, new Tile(i, j, TileType.WALL))
      }
    }
    
  }
  
  //* Helper function for roundEdges method*/
  def countAliveNeighbours(tile: Tile): Int = {
    var num = 0
    for (i <- Range(-1,2))
      for (j <- Range(-1,2))
        if (i == 0 && j == 0) {}
        else if (!isWithinGrid(tile.getX+i, tile.getY+j)) {}
        else if (getTile(tile.getX+i,tile.getY+j).getType == TileType.FLOOR) num += 1
    num
  }
  
  /** Make testmap */
  def testMap() {
    for (i <- Range(0,size)) {
      for (j <- Range(0,size)) {
        map(i)(j) = new Tile(i, j, 
            if (rnd.nextInt(100) < 10) TileType.WALL 
            else if (i == 0 || j == 0 || i + 1 == size || j + 1 == size) TileType.WALL 
            else TileType.FLOOR)
      }
    }
    addStairs()
    getPlayer.setX(size/2)
    getPlayer.setY(size/2)
  }
  
  /** getter for stairs */
  def getStairs() = stairs
  
  /** Add stairs */
  def addStairs() {
    var coord: Tuple2[Int, Int] = (0, 0)
    if (stairs.getX != -100) setTile(stairs.getX, stairs.getY, new Tile(stairs.getX, stairs.getY, TileType.FLOOR))
    do {
      coord = giveRandomNonBlockinCoordinates
      stairs = new Tile(giveRandomNonBlockinCoordinates._1, giveRandomNonBlockinCoordinates._2, TileType.STAIRS)
    }
    while ((stairs.getX > size/3 && stairs.getX < size*2/3) || (stairs.getY > size/3 && stairs.getY < size*2/3))
    setTile(stairs.getX, stairs.getY, stairs)
  }  
  
  /** Returns the neighbors of given tile */
  def neighbors(tile: Tile):Buffer[Tile] = {
    val list = Buffer[Tile]()
    if (isWithinGrid(tile.getX()+1, tile.getY)) list.append(getTile(tile.getX()+1, tile.getY))
    if (isWithinGrid(tile.getX()-1, tile.getY)) list.append(getTile(tile.getX()-1, tile.getY))
    if (isWithinGrid(tile.getX(), tile.getY+1)) list.append(getTile(tile.getX(), tile.getY+1))
    if (isWithinGrid(tile.getX(), tile.getY-1)) list.append(getTile(tile.getX(), tile.getY-1))
    list
  }
  
  /** Give random x/y-coordinate from the map */
  def randomX() = rnd.nextInt(size-2)+1
  def randomY() = randomX
  
  /** Returns true if given coordinates are on the map */
  def isWithinGrid(x: Int, y: Int): Boolean = x < size && x >= 0 && y < size && y >= 0
  
  /** Setter to handle the map */
  def setTile(x: Int, y: Int, tile: Tile) = map(x)(y) = tile
  
  /** Getter to handle the map, note that will return null if tile at the given location doesn't exist */
  def getTile(x: Int, y: Int) = if (isWithinGrid(x, y)) map(x)(y) else map(1)(1)
  
  /** Draw the whole map */
  def draw() {
    for (i <- Range(0,size)) {
      for (j <- Range(0,size)) {
        if (getTile(i, j).xDif(getPlayer) <= 16 && getTile(i, j).yDif(getPlayer) <= 8) 
          map(i)(j).draw
      }
    }
  }
  
  /** Calculate distance between two points */
  def distance(tile1: Tile, tile2: Tile): Int = tile1.distance(tile2)
  
  /** Returns random nonblocking coordinates */
  def giveRandomNonBlockinCoordinates(): Tuple2[Int, Int] = {
    val tile = giveRandomNonBlockingTile 
    (tile.getX, tile.getY)
  }
  
  /** Returns one random nonblocking tile */
  def giveRandomNonBlockingTile(): Tile = {
    var tile: Tile = null
    do {tile = getTile(randomX, randomY)}
    while (tile.blockMovement)
    tile
  }
  
  /** Find's out if the map is one big area or two (or more) separate ones */
  def mapIsContinuous(): Boolean = {
    var checkList = Map[Int, Tile]()
    var patternNumber: Int = 0
    
    for (i <- Range(0,size)) {
      for (j <- Range(0,size)) {
        map(i)(j).label = 0
      }
    }
    
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