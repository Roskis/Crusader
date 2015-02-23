package game

import collection.mutable.Buffer
import Math.{sqrt, abs, cos, sin}
import Main._
import Direction._

/** Simple coordinate system */
class Coordinate(var x: Int, var y: Int) {
  
  /** Getters and setters for coordinates */
  def getX = x
  def getY = y
  def setX(newX: Int) = x = newX
  def setY(newY: Int) = y = newY
  
}

/** Grid is reponsible for handling the map */
class Grid() {
  private val rnd = getRnd
  private var size: Int = 100
  private var map = Array.ofDim[Tile](size, size)
  private var stairs: Tile = new Tile(-100, -100, TileType.STAIRS)
  private var altar: Object = new PassiveObject("Altar", "Make your sacrifices here.", -100, -100, 
    "tempAltar")
  var djinn = new PassiveObject("Djinn", "merchant", -100, -100, "tempDjinn")
  
  init()
  
  /** Different levels make different maps */
  def init() {
    if (getLevel < 5) {
      size = rnd.nextInt(5)+40
      map = Array.ofDim[Tile](size, size)
      do {
        clearLists
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
  map(getPlayer.getX)(getPlayer.getY).explored = true
  }
  
  /** Make first episode map.
   *
   * @param startProcent defines starting floor area
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
    roundEdges(birthlimit, deathlimit)
    
    if (rnd.nextInt(4) != 0) addShop
    else {
      djinn.setX(-100)
      djinn.setY(-100)
    }
    if (rnd.nextInt(5) != 0) addAltar
    else {
      altar.setX(-100)
      altar.setY(-100)
    }
    addStairs
    addtrees(16)
    addtrees(4)
    
    var playerPosition = giveRandomNonBlockinCoordinates
    do {
      playerPosition = giveRandomNonBlockinCoordinates
      getPlayer.setX(playerPosition.getX)
      getPlayer.setY(playerPosition.getY)
    }
    while (getPlayer.distance(getStairs) < 15 || getTile(playerPosition.getX, playerPosition.getY).getType != TileType.FLOOR)
    map(getPlayer.getX)(getPlayer.getY).explored = true
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
        if (map(i)(j).label == 1) setTile(new Tile(i, j, TileType.FLOOR))
        else setTile(new Tile(i, j, TileType.WALL))
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
  
  /** Adds shop to the map */
  def addShop() {
    var coord = new Coordinate(randomX, randomY)
    var door = new Tile(-100, -100, TileType.DJINNDOORH)
    var direction: Direction = Direction.N
    
    do {
      direction = randomDirection(4)
      coord = new Coordinate(randomX, randomY)
    }
    while (!isWithinGrid(coord.getX-4, coord.getY-4) || !isWithinGrid(coord.getX+4, coord.getY-4) || 
        !isWithinGrid(coord.getX+4, coord.getY+4) || !isWithinGrid(coord.getX-4, coord.getY+4))
      
    for (x <- Range(coord.getX-3, coord.getX+4)) {
      for (y <- Range(coord.getY-3, coord.getY+4)) {
        if (map(x)(y).distance(coord.getX, coord.getY) > 2 && map(x)(y).distance(coord.getX, coord.getY) < 3.5) map(x)(y) = new Tile(x, y, TileType.DJINNWALL)
        else if (map(x)(y).distance(coord.getX, coord.getY) <= 2) map(x)(y) = new Tile(x, y, TileType.DJINNFLOOR)
      }
    }
    
    var slotList = Buffer[Coordinate] (new Coordinate(coord.getX-1, coord.getY-1), 
        new Coordinate(coord.getX-1, coord.getY+1), new Coordinate(coord.getX+1, coord.getY+1), 
        new Coordinate(coord.getX+1, coord.getY-1), new Coordinate(coord.getX-2, coord.getY), 
        new Coordinate(coord.getX+2, coord.getY), new Coordinate(coord.getX, coord.getY-2), 
        new Coordinate(coord.getX, coord.getY+2))
        
    direction match {
      case d if (d == N) => {
        door.setX(coord.getX)
        door.setY(coord.getY-3)
        slotList.filter(_.getY ==  coord.getY-2) foreach {slotList -= _}
        slotList.filter(_.getY == coord.getY+2) foreach {slotList -= _}
        djinn.setX(coord.getX)
        djinn.setY(coord.getY+2)
      }
      case d if (d == E) => {
        door.setX(coord.getX+3)
        door.setY(coord.getY)
        slotList.filter(_.getX == coord.getX-2) foreach {slotList -= _}
        slotList.filter(_.getX == coord.getX+2) foreach {slotList -= _}
        djinn.setX(coord.getX-2)
        djinn.setY(coord.getY)
      }
      case d if (d == S) => {
        door.setX(coord.getX)
        door.setY(coord.getY+3)
        slotList.filter(_.getY == coord.getY-2) foreach {slotList -= _}
        slotList.filter(_.getY == coord.getY+2) foreach {slotList -= _}
        djinn.setX(coord.getX)
        djinn.setY(coord.getY-2)
      }
      case d if (d == W) => {
        door.setX(coord.getX-3)
        door.setY(coord.getY)
        slotList.filter(_.getX == coord.getX+2) foreach {slotList -= _}
        slotList.filter(_.getX == coord.getX-2) foreach {slotList -= _}
        djinn.setX(coord.getX+2)
        djinn.setY(coord.getY)
      }
    }
    for (tile <- slotList) {
      setTile(new Tile(tile.getX, tile.getY, TileType.DJINNSLOT))
    }
    setTile(new Tile(door.getX, door.getY, 
        if(direction == E || direction == W) TileType.DJINNDOORH else TileType.DJINNDOORV))
  }
  
  /** Add a number of trees */
  def addtrees(num: Int) = {
    var tree: PassiveObject = null
    var i = 0
    var coord: Coordinate = new Coordinate(-100, -100)
    do {
      do {
        coord = giveRandomNonBlockinCoordinates
      }
      while (!(getTile(coord.getX, coord.getY).getType == TileType.FLOOR))
      tree = new PassiveObject("Tree", "Large generic tree", coord.getX, coord.getY, "Items/tempTree")
      tree.blockVision = true
      getTile(tree.getX, tree.getY).blockVision = true
      i += 1
    }
    while (i < num)
  }
  
  /** Add a number of rocks */
  def addrocks(num: Int) = {
    var rock: PassiveObject = null
    var i = 0
    var coord: Coordinate = new Coordinate(-100, -100)
    do {
      do {
        coord = giveRandomNonBlockinCoordinates
      }
      while (!(getTile(coord.getX, coord.getY).getType == TileType.FLOOR))
      rock = new PassiveObject("Rock", "Large boulder", coord.getX, coord.getY, "Items/tempRock")
      rock.blockVision = true
      getTile(rock.getX, rock.getY).blockVision = true
      i += 1
    }
    while (i < num)
  }
  
  /** getter for altar */
  def getAltar() = altar
  
  /** Add altar */
  def addAltar() = {
    var coord: Coordinate = new Coordinate(-100, -100)
    do {
      coord = giveRandomNonBlockinCoordinates
    }
    while (!(getTile(coord.getX, coord.getY).getType == TileType.FLOOR))
    altar.setX(coord.getX)
    altar.setY(coord.getY)
  }
  
  /** getter for stairs */
  def getStairs() = stairs
  
  /** Add stairs */
  def addStairs() {
    var coord: Coordinate = new Coordinate(-100, -100)
    do {
      coord = giveRandomNonBlockinCoordinates
      stairs.setX(coord.getX)
      stairs.setY(coord.getY)
    }
    while ((stairs.getX > size/3 && stairs.getX < size*2/3) || (stairs.getY > size/3 && stairs.getY < size*2/3) || getTile(stairs.getX, stairs.getY).getType != TileType.FLOOR)
    setTile(stairs)
  }  
  
  /** Returns the neighbors of given tile */
  def neighbors(tile: Tile):Buffer[Tile] = {
    val list = Buffer[Tile]()
    var coord: Coordinate = new Coordinate(-100, -100)
    for (dir <- List(N, E, S, W))  {
      coord = getCoordinates(dir, tile.getX, tile.getY)
      if (isWithinGrid(coord.getX, coord.getY)) list.append(getTile(coord.getX, coord.getY))
    }
    list
  }
  
  /** Give random x/y-coordinate from the map */
  def randomX() = rnd.nextInt(size-2)+1
  def randomY() = randomX
  
  /** Returns true if given coordinates are on the map */
  def isWithinGrid(x: Int, y: Int): Boolean = x < size && x >= 0 && y < size && y >= 0
  
  /** Setter to handle the map */
  def setTile(tile: Tile) = map(tile.getX)(tile.getY) = tile
  
  /** Getter to handle the map, note that will return null if tile at the given location doesn't exist */
  def getTile(x: Int, y: Int) = if (isWithinGrid(x, y)) map(x)(y) else map(1)(1)
  
  /** Getter for size */
  def getSize() = size
  
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
  def distance(tile1: Tile, tile2: Tile): Double = tile1.distance(tile2)
  
  /** TODO */
  def FOV() = {
    var x: Float = 0
    var y: Float = 0
    hideMap
    map(getPlayer.getX)(getPlayer.getY).visible = true
    for (i <- Range(0,360)) {
      x = cos(i.toFloat*0.01745f).toFloat
      y = sin(i.toFloat*0.01745f).toFloat
      FOVRay(x,y)
    }
  }
  
  /** TODO */
  def FOVRay(x: Float, y: Float) {
    var ox: Float = getPlayer.getX.toFloat + 0.5f + x
    var oy: Float = getPlayer.getY.toFloat + 0.5f + y
    for (i <- Range(0, getPlayer.viewRadius)) {
      map(ox.toInt)(oy.toInt).visible = true
      map(ox.toInt)(oy.toInt).explored = true
      if (map(ox.toInt)(oy.toInt).blockVision) return
      ox += x
      oy += y
    }
  }
  
  /** TODO */
  def hideMap() = {
    for (i <- Range(0,size)) {
      for (j <- Range(0,size)) {
        map(i)(j).visible = false
      }
    }
  }
  
  /** Uses Bresenham's Line Algorithm to calculate straight line between two coordinates.
   *
   * @param coord1 is first given coordinate
   * @param coord2 is second given coordinate
   * @return Buffer containing coordinates between the two points (including starting and end point)
   */
  def line(coord1: Coordinate, coord2: Coordinate): Buffer[Coordinate] = {
    val points = Buffer[Coordinate]()
    var x1 = coord1.getX
    var y1 = coord1.getY
    var x2 = coord2.getX
    var y2 = coord2.getY
    var dx = abs(x2-x1)
    var dy = abs(y2-y1)
    var sx = if (x1 < x2) 1 else -1
    var sy = if (y1 < y2) 1 else -1
    var err = dx - dy
    points.append(new Coordinate(x1, y1))
    while (!((x1 == x2) && (y1 == y2))) {
      var e2 = err << 1
      if (e2 > -dy) {
        err -= dy
        x1 += sx
      }
      if (e2 < dx) {
        err += dx
        y1 += sy
      }
      points.append(new Coordinate(x1, y1))
    }
    points
  }
  
  /** Returns random nonblocking coordinates */
  def giveRandomNonBlockinCoordinates(): Coordinate = {
    val tile = giveRandomNonBlockingTile 
    new Coordinate(tile.getX, tile.getY)
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