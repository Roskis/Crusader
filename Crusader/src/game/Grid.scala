package game

import java.lang.Math.{abs, cos, sin}

import scala.Range
import scala.collection.mutable.Buffer

import Direction._
import Main._
import Helpers._
import PassiveType._
import MonsterType._
import Extra._

/** Grid is reponsible for handling the map */
class Grid() extends Serializable {
  private val rnd = getRnd
  private var size: Int = 1 // maximum size 49x49 or minimap won't work
  private var map = Array.ofDim[Tile](size, size)
  private var stairs: PassiveObject = null
  private var altar: PassiveObject = null
  private var djinn: Monster = null
  var secretTile: Tile = null
  val shopImageNumbers = (rnd.nextInt(2)+1, rnd.nextInt(3)+1, rnd.nextInt(3)+1) //first is shopkeeper, second is walls and third is floor
  
  /** Different levels make different maps */
  def init() {
    if (getLevel < 5) {
      size = rnd.nextInt(5)+40
      map = Array.ofDim[Tile](size, size)
      do {
        clearLists
        altar = new PassiveObject("Altar", "TODO", -100, -100, ALTAR1)
        djinn = new Monster(-100, -100, DJINN)
        djinn.name = djinnName
        makeMap1(45, 4, 4)
      }
      while (!mapIsContinuous)
    }
    else if (getLevel == 5) {
      size = 27
      map = Array.ofDim[Tile](size, size)
      altar = new PassiveObject("Altar", "TODO", -100, -100, ALTAR2)
      djinn = new Monster(-100, -100, DJINN)
      djinn.name = djinnName
      makeBoss1
    }
    else {
      size = 10
      map = Array.ofDim[Tile](size, size)
      altar = new PassiveObject("Altar", "TODO", -100, -100, ALTAR1)
      djinn = new Monster(-100, -100, DJINN)
      djinn.name = djinnName
      testMap
    }
  }

  /** Temporary bossmap */
  def makeBoss1() {
    map(size/2)(size/2-3) = new Tile(size/2, size/2-3, TileType.FLOOR)
    for (i <- Range(0,size)) {
      for (j <- Range(0,size)) {
        setTile(new Tile(i, j, if (map(size/2)(size/2-3).distance(new Coordinate(i, j)) <= 9) TileType.FLOOR else TileType.WALL)
        )
      }
    }

    for (i <- Range(9,17)) {
      for (j <- Range(21,26)) {
        if (i < 13) getTile(i,j).tileType = TileType.DJINNFLOOR else getTile(i,j).tileType = TileType.FLOOR
      }
    }
    
    getTile(13,20).tileType = TileType.BOSSDOOR1
    getTile(13, 20).addObject(new PassiveObject("Gate", "TODO", 13, 20, PassiveType.GATE1))
    addGrass(getTile(13, 20))
    getTile(13,19).tileType = TileType.BOSSDOOR2
    addGrass(getTile(13, 19))
    getTile(16,21).tileType = TileType.WALL
    getTile(16,25).tileType = TileType.WALL
    altar.setX(15)
    altar.setY(23)
    getTile(15, 23).addObject(altar)
    
    getTile(9,21).tileType = TileType.WALL
    getTile(9,25).tileType = TileType.WALL
    
    djinn.setX(11)
    djinn.setY(23)
    getTile(11, 23).addObject(djinn)
    
    addShopItem(new Coordinate(10, 21)).inShop=true
    addShopItem(new Coordinate(11, 21)).inShop=true
    addShopItem(new Coordinate(12, 21)).inShop=true
    addShopItem(new Coordinate(9, 22)).inShop=true
    addShopItem(new Coordinate(9, 23)).inShop=true
    addShopItem(new Coordinate(9, 24)).inShop=true
    addShopItem(new Coordinate(10, 25)).inShop=true
    addShopItem(new Coordinate(11, 25)).inShop=true
    addShopItem(new Coordinate(12, 25)).inShop=true
    
    getPlayer.setX(13)
    getPlayer.setY(23)
    map(getPlayer.getX)(getPlayer.getY).explored = true
    
    new Monster(13, 9, MonsterType.SLOTH)

    addFloorExtras
    
    for (i <- Range(12,17)) {
      for (j <- Range(21,26)) {
        getTile(i,j).objectList.filter(_.blockMovement) foreach {getTile(i,j).objectList -= _}
        getTile(i,j).objectList.filter(_.blockVision) foreach {getTile(i,j).objectList -= _}
      }
    }
    
  }
  
  /** Make first episode map.
   *
   * @param startProcent defines starting floor area
   * @param birthlimit and deathlimit change how cellular automata work for map
   */
  def makeMap1(startProcent: Int, birthlimit: Int, deathlimit: Int) = {
    for (i <- Range(0,size)) {
      for (j <- Range(0,size)) {
        setTile(new Tile(i, j, 
            if (rnd.nextInt(100) < startProcent) TileType.WALL 
            else if (i == 0 || j == 0 || i + 1 == size || j + 1 == size) TileType.WALL 
            else TileType.FLOOR)
        )
      }
    }
    roundEdges(birthlimit, deathlimit)
    roundEdges(birthlimit, deathlimit)
    roundEdges(birthlimit, deathlimit)
    roundEdges(birthlimit, deathlimit)
    
    addShop
    getTile(djinn.getX, djinn.getY).addObject(djinn)
    addAltar
    getTile(altar.getX, altar.getY).addObject(altar)
    addStairsEp1
    
    do {movePlayerEp1}
    while (!(getTile(getPlayer.getX, getPlayer.getY)).getObjectList.isEmpty || 
        !(getTile(getPlayer.getX, getPlayer.getY).tileType == TileType.WALL || 
            getTile(getPlayer.getX, getPlayer.getY).tileType == TileType.FLOOR))
    setTile(new Tile(getPlayer.getX, getPlayer.getY, TileType.FLOOR))
    map(getPlayer.getX)(getPlayer.getY).explored = true
    
    addMonsters(20)
    makeSecret
    addFloorExtras
    
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
        setTile(new Tile(i, j, 
            if (rnd.nextInt(100) < 10) TileType.WALL 
            else if (i == 0 || j == 0 || i + 1 == size || j + 1 == size) TileType.WALL 
            else TileType.FLOOR)
        )
      }
    }
    addStairsEp1
    addMonsters(400)
    getPlayer.setX(size/2)
    getPlayer.setY(size/2)
  }
  
  /** getter for djinn */
  def getDjinn() = djinn
  
  /** Adds shop to the map */
  def addShop() = {
    var coord: Coordinate = null
    var door: Tile = new Tile(-100, -100, TileType.DJINNDOORH)
    var direction: Direction = null
    
    do {
      direction = randomDirection(4)
      coord = new Coordinate(randomX, randomY)
    }
    while (!isWithinGrid(coord.getX-4, coord.getY-4) || !isWithinGrid(coord.getX+4, coord.getY-4) || 
        !isWithinGrid(coord.getX+4, coord.getY+4) || !isWithinGrid(coord.getX-4, coord.getY+4) || 
        getTile(getCoordinates(direction, getCoordinates(direction, getCoordinates(direction, 
            coord)))).getType != TileType.FLOOR)
      
    for (x <- Range(coord.getX-3, coord.getX+4)) {
      for (y <- Range(coord.getY-3, coord.getY+4)) {
        if (map(x)(y).distance(coord.getX, coord.getY) > 2 && 
            map(x)(y).distance(coord.getX, coord.getY) < 3.5) setTile(new Tile(x, y, TileType.DJINNWALL))
        else if (map(x)(y).distance(coord.getX, coord.getY) <= 2) setTile(new Tile(x, y, TileType.DJINNFLOOR))
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
      addShopItem(new Coordinate(tile.getX, tile.getY)).inShop=true
    }
    setTile(new Tile(door.getX, door.getY, 
        if(direction == E || direction == W) TileType.DJINNDOORH else TileType.DJINNDOORV))
  }
    
  /** Add monsters */
  def addMonsters(num: Int) = {
    var coord: Coordinate = null
    for (n <- Range(0, num)) {
      do coord = giveRandomNonBlockingCoordinates
      while (getTile(coord.getX, coord.getY).getType != TileType.FLOOR || 
          !(getTile(coord.getX, coord.getY)).getObjectList.isEmpty || 
          getPlayer.distance(coord) < 5)
      new Monster(coord.getX, coord.getY, chooseRandomMonster(getMonsterChances))
    }
  }
  
  /** getter for altar */
  def getAltar() = altar
  
  /** Add altar */
  def addAltar() = {
    var coord: Coordinate = null
    do coord = giveRandomNonBlockingCoordinates
    while (!(getTile(coord.getX, coord.getY).getType == TileType.FLOOR) || !(getTile(coord.getX, coord.getY)).getObjectList.isEmpty)
    altar.setX(coord.getX)
    altar.setY(coord.getY)
  }
  
  /** Add random item */
  def addItem(coord: Coordinate) = {
    val itemType = chooseRandomItem(getItemChances)
    var item: Item = null
    if (ItemType.slot(itemType) == "item") item = new Useable(coord.getX, coord.getY, itemType, false)
    else item = new Equipment(coord.getX, coord.getY, itemType, false)
    item
  }
  
  /** Add random item */
  def addShopItem(coord: Coordinate) = {
    val itemType = chooseRandomItem(getShopChances)
    var item: Item = null
    if (ItemType.slot(itemType) == "item") item = new Useable(coord.getX, coord.getY, itemType, false)
    else item = new Equipment(coord.getX, coord.getY, itemType, false)
    item
  }
  
  /** getter for stairs */
  def getStairs() = stairs
  
  /** Add player to the map */
  def movePlayerEp1() = {
    val startTile = getTile(1, rnd.nextInt(size-10)+5)
    var boo = true
    val tunnel = line(startTile, getTile(size/2, size/2))
    var n = 0
    var next = getTile(tunnel(n))
    var current = startTile
    var neigh = neighbors(startTile, 8)
    do {
      boo = true
      current = next
      n+= 1
      next = getTile(tunnel(n))
      neigh = neighbors(next, 8)
      neigh -= current
      for (ne <- neigh) if (!ne.blockMovement) boo = false
    }
    while (boo)
    getPlayer.setX(next.getX)
    getPlayer.setY(next.getY)
  }
  
  /** Add stairs */
  def addStairsEp1() = {
    val startTile = getTile(size-1, rnd.nextInt(size))
    var boo = true
    val tunnel = line(startTile, getTile(size/2, size/2))
    var n = 0
    var next = getTile(tunnel(n))
    var current = startTile
    var neigh = neighbors(startTile, 8)
    do {
      boo = true
      current = next
      n+= 1
      next = getTile(tunnel(n))
      neigh = neighbors(next, 8)
      neigh -= current
      for (ne <- neigh) if (!ne.blockMovement) boo = false
    }
    while (boo)
    setTile(new Tile(next.getX, next.getY, TileType.FLOOR))
    stairs = new PassiveObject("Portal", "TODO", next.getX, next.getY, STAIRS)
  }
  
  /** Move stairs to player's location */
  def moveStairsToPlayer = {
    val tile = giveTileNearPlayer(5)
    stairs.changePosition(tile.getX, tile.getY)
    tile.explored = true
  }
  
  /** Add secret to the level */
  def makeSecret {
    var boo = false
    secretTile = getTile(randomX, randomY)
    var neigh = neighbors(secretTile, 8)
    do {
      boo = false
      secretTile = getTile(randomX, randomY)
      neigh = neighbors(secretTile, 8)
      for (n <- neigh) if (!n.blockMovement) boo = true
      }
    while (boo || secretTile.distance(getPlayer) < 5)
    val goto = giveRandomFloor
    val tunnel = line(secretTile, goto)
    var n = 0
    var next = getTile(tunnel(n))
    var current = secretTile
    do {
      boo = true
      current = next
      setTile(new Tile(current.getX, current.getY, TileType.FLOOR))
      n+= 1
      next = getTile(tunnel(n))
      neigh = neighbors(next, 8)
      neigh -= current
      for (ne <- neigh) if (!ne.blockMovement) boo = false
    }
    while (boo)
    setTile(new Tile(next.getX, next.getY, TileType.SECRETDOOR))
    for (tile <- neighbors(secretTile, 8)) if (tile.getX != secretTile.getX && 
        tile.getY != secretTile.getY && tile.tileType == TileType.FLOOR) tile.tileType = TileType.WALL
    addItem(new Coordinate(secretTile.getX, secretTile.getY))
  }
  
  /** Add some extra to the floortiles */
  def addFloorExtras() = {
    var currentTile: Tile = null
    for (i <- Range(0, size)) {
      for (j <- Range(0, size)) {
        if (getTile(i,j).getType == TileType.FLOOR) {
          currentTile = getTile(i, j)
          addGrass(currentTile)
          if (currentTile.objectList.isEmpty) {
            rnd.nextInt(1000) match {
              case t if (t < 100) => addBush(currentTile)
              case t if (t < 180) => addFlower(currentTile)
              case t if (t < 220) => addfern(currentTile)
              case t if (t < 240) => addsmallRock(currentTile)
              case t if (t < 260) => addBigFlower(currentTile)
              case t if (t < 280) => {
                addFlower(currentTile)
                addFlower(currentTile)
              }
              case t if (t < 300) => {
                addBush(currentTile)
                addBush(currentTile)
              }
              case t if (t < 320) => {
                addBush(currentTile)
                addFlower(currentTile)
              }
              case t if (t < 340) => addTree(currentTile)
              case t if (t < 345) => addBigRock(currentTile)
              case t if (t < 350) => addBigBush(currentTile)
              case t if (t < 351) => addLake(currentTile)
              case _ => {}
            } 
          }
        }
      }
    }
  }
  
  def addGrass(tile: Tile) = {
    tile.addExtra(
      rnd.nextInt(3) match {
        case r if (r == 0) => GRASS1
        case r if (r == 1) => GRASS2
        case r if (r == 2) => GRASS3
      }, 0, 0)
    }
  
  def addFlower(tile: Tile) = {
    tile.addExtra(
      rnd.nextInt(11) match {
        case r if (r == 0) => FLOWER1
        case r if (r == 1) => FLOWER2
        case r if (r == 2) => FLOWER3
        case r if (r == 3) => FLOWER4
        case r if (r == 4) => FLOWER5
        case r if (r == 5) => FLOWER6
        case r if (r == 6) => FLOWER7
        case r if (r == 7) => FLOWER8
        case r if (r == 8) => FLOWER9
        case r if (r == 9) => FLOWER10
        case r if (r == 10) => FLOWER11
      }, rnd.nextInt(24)-4, rnd.nextInt(24)-4)
  }
  
  def addLake(tile: Tile) = {
    tile.addExtra(
      rnd.nextInt(4) match {
        case r if (r == 0) => LAKE1
        case r if (r == 1) => LAKE2
        case r if (r == 2) => LAKE3
        case r if (r == 3) => LAKE4
      }, rnd.nextInt(16), rnd.nextInt(16))
  }
  
  def addBigFlower(tile: Tile) = {
    tile.addExtra(
      rnd.nextInt(6) match {
        case r if (r == 0) => BIGFLOWER1
        case r if (r == 1) => BIGFLOWER2
        case r if (r == 2) => BIGFLOWER3
        case r if (r == 3) => BIGFLOWER4
        case r if (r == 4) => BIGFLOWER5
        case r if (r == 5) => BIGFLOWER6
      }, 0, 0)
  }
  
  def addBush(tile: Tile) = {
    tile.addExtra(
      rnd.nextInt(7) match {
        case r if (r == 0) => BUSH1
        case r if (r == 1) => BUSH2
        case r if (r == 2) => BUSH3
        case r if (r == 3) => BUSH4
        case r if (r == 4) => BUSH5
        case r if (r == 5) => BUSH6
        case r if (r == 6) => BUSH7
      }, rnd.nextInt(16), rnd.nextInt(20)-2)
  }
  
  def addBigRock(tile: Tile) = {
    new PassiveObject("Rock", "TODO", tile.getX, tile.getY, rnd.nextInt(2) match {
        case r if (r == 0) => ROCK1
        case r if (r == 1) => ROCK2
      })
  }
  
  def addsmallRock(tile: Tile) = {
    tile.addExtra(
      rnd.nextInt(7) match {
        case r if (r == 0) => SMALLROCK1
        case r if (r == 1) => SMALLROCK2
        case r if (r == 2) => SMALLROCK3
        case r if (r == 3) => SMALLROCK4
        case r if (r == 4) => SMALLROCK5
        case r if (r == 5) => SMALLROCK6
        case r if (r == 6) => SMALLROCK7
      }, rnd.nextInt(16), rnd.nextInt(16))
  }
  
  def addBigBush(tile: Tile) = {
    tile.addExtra(
      rnd.nextInt(4) match {
        case r if (r == 0) => BIGBUSH1
        case r if (r == 1) => BIGBUSH2
        case r if (r == 2) => BIGBUSH3
        case r if (r == 3) => BIGBUSH4
      }, 0, 0)
  }
  
  def addfern(tile: Tile) = {
    tile.addExtra(
      rnd.nextInt(4) match {
        case r if (r == 0) => FERN1
        case r if (r == 1) => FERN2
        case r if (r == 2) => FERN3
        case r if (r == 3) => FERN4
      }, rnd.nextInt(16), rnd.nextInt(16))
  }
  
  def addTree(tile: Tile) = {
    var tree: PassiveObject = null
    if (rnd.nextBoolean) {
        tree = new PassiveObject("Tree", "TODO", tile.getX, tile.getY, BIGTREE1)
        tree.blockMovement = true
      }
      else tree = new PassiveObject("Tree", "TODO", tile.getX, tile.getY, TREE1)
      tree.blockVision = true
  }
  
  /** Returns the neighbors of given tile */
  def neighbors(tile: Tile, num: Int):Buffer[Tile] = {
    val list = Buffer[Tile]()
    var coord: Coordinate = new Coordinate(-100, -100)
    if (num == 4) {
      for (dir <- List(N, E, S, W)) {
        coord = getCoordinates(dir, tile.getX, tile.getY)
        if (isWithinGrid(coord.getX, coord.getY)) list.append(getTile(coord.getX, coord.getY))
      }
    }
    else if (num == 8) {
      for (dir <- List(N, E, S, W, NE, NW, SE, SW)) {
        coord = getCoordinates(dir, tile.getX, tile.getY)
        if (isWithinGrid(coord.getX, coord.getY)) list.append(getTile(coord.getX, coord.getY))
      }
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
  
  /** Getter to handle the map, note that will return (x = 1, y = 1) if tile at the given location doesn't exist */
  def getTile(x: Int, y: Int): Tile = if (isWithinGrid(x, y)) map(x)(y) else null
  
  /** Getter to handle the map, note that will return (x = 1, y = 1) if tile at the given location doesn't exist */
  def getTile(coord: Coordinate): Tile = getTile(coord.getX, coord.getY)
  
  /** Getter for size */
  def getSize() = size
  
  def getTiles(): Buffer[Tile] = {
    val buff = Buffer[Tile]()
    for (j <- Range(0,size)) {
      for (i <- Range(0,size)) {
        buff.append(getTile(j,i))
      }
    }
    buff
  }
  
  /** Draw the whole map */
  def draw() {
    for (j <- Range(0,size)) {
      for (i <- Range(0,size)) {
        if (getTile(i, j).xDif(getPlayer) <= 16 && getTile(i, j).yDif(getPlayer) <= 8) map(i)(j).draw
      }
    }
    for (j <- Range(0,size)) {
      for (i <- Range(0,size)) {
        if (getTile(i, j).xDif(getPlayer) <= 16 && getTile(i, j).yDif(getPlayer) <= 8) map(i)(j).drawExtra
      }
    }
    for (j <- Range(0,size)) {
      for (i <- Range(0,size)) {
        if (getTile(i, j).xDif(getPlayer) <= 16 && getTile(i, j).yDif(getPlayer) <= 8) map(i)(j).drawObjects
      }
    }
  }
  
  /** Calculate distance between two points */
  def distance(tile1: Tile, tile2: Tile): Double = tile1.distance(tile2)
  
  /** Simple ray casting method to draw field of vision */
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
  
  /** Single ray used by FOV */
  def FOVRay(x: Float, y: Float) {
    var ox: Float = getPlayer.getX.toFloat + 0.5f + x
    var oy: Float = getPlayer.getY.toFloat + 0.5f + y
    for (i <- Range(0, getPlayer.viewRadius.toInt)) {
      map(ox.toInt)(oy.toInt).visible = true
      map(ox.toInt)(oy.toInt).explored = true
      if (map(ox.toInt)(oy.toInt).blockVision) return
      ox += x
      oy += y
    }
  }
  
  /** Make whole level not visible */
  def hideMap() = {
    for (i <- Range(0,size)) {
      for (j <- Range(0,size)) {
        map(i)(j).visible = false
      }
    }
  }
  
  /** Hide whole map */
  def unexploreAll = {
    for (i <- Range(0,size)) {
      for (j <- Range(0,size)) {
        map(i)(j).explored = false
      }
    }
    getTile(getPlayer.getX, getPlayer.getY).explored = true
  }
  
  /** Reveal whole map */
  def exploreAll = {
    var boo = false
    for (i <- Range(0,size)) {
      for (j <- Range(0,size)) {
        boo = false
        for (n <- neighbors(map(i)(j), 8)) if (n.getType == TileType.FLOOR) boo = true
        if (map(i)(j).getType == TileType.FLOOR || boo) map(i)(j).explored = true
      }
    }
  }
  
  /** Alternative line */
  def line(tile1: Tile, tile2: Tile): Buffer[Coordinate] = line(new Coordinate(tile1.getX, tile1.getY), new Coordinate(tile2.getX, tile2.getY))
  
  /** Alternative line */
  def line(x1: Int, y1: Int, x2: Int, y2: Int): Buffer[Coordinate] = line(new Coordinate(x1, y1), new Coordinate(x2, y2))
  
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
  
  /** Returns random floor tile near player within range */
  def giveTileNearPlayer(maxRange: Int): Tile = {
    var tile = giveRandomFloor
    do tile = giveRandomFloor
    while (tile.distance(getPlayer) > maxRange || tile.distance(getPlayer) < 1)
    tile
  }
  
  /** Returns random floor */
  def giveRandomFloor(): Tile = {
    var tile: Tile = new Tile(-100, -100, TileType.WALL)
    do tile = getTile(randomX, randomY)
    while (tile.getType != TileType.FLOOR)
    tile
  }
  
  /** Returns random nonblocking coordinates */
  def giveRandomNonBlockingCoordinates(): Coordinate = {
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
  
  /** A star search algorithm to find route between points */
  def dijkstra(start: Tile, end: Tile): Buffer[Tile] = {
    var open = Buffer[Tile](start)
    var visited = Buffer[(Tile, Tile)]((start, null))
    var current: Tile = null
    var goalFound = false
    while (!(open.isEmpty || goalFound)) {
      current = open.head
      if (current == end) goalFound = true
      open = open.drop(1)
      for (neighbor <- neighbors(current, 8)) {
        if (!visited.unzip._1.contains(neighbor) && !neighbor.blockMovement) {
          open.append(neighbor)
          visited.append((neighbor, current))
        }
        else if (neighbor == end) visited.append((neighbor, current))
      }
    }
    current = end
    var path = Buffer[Tile](current)
    var index = 0
    while (current != start) {
      index = visited.unzip._1.indexOf(current)
      if (index <= 0) return Buffer[Tile]()
      current = visited.unzip._2(index)
      path.append(current)
    }
    path.reverse
  }
  
  /** Reset all labels of tiles */
  def resetTiles {
  for (i <- Range(0,size)) {
    for (j <- Range(0,size)) {
      map(i)(j).label = 0
      }
    }
  }
  
  /** Find's out if the map is one big area or two (or more) separate ones */
  def mapIsContinuous(): Boolean = {
    var checkList = Map[Int, Tile]()
    var patternNumber: Int = 0
    resetTiles
    for (i <- Range(0,size)) {
      for (j <- Range(0,size)) {
        if (!map(i)(j).blockMovement) {
          map(i)(j).label = size*size+1
          for (near <- neighbors(map(i)(j), 8)) {
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
          for (near <- neighbors(map(n)(m), 8)) 
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