package game

import java.awt.Font
import java.io.IOException
import java.io.InputStream

import scala.Range
import scala.collection.mutable.Buffer

import org.lwjgl.LWJGLException
import org.lwjgl.input.Mouse
import org.lwjgl.opengl.Display
import org.lwjgl.opengl.DisplayMode
import org.lwjgl.opengl.GL11.GL_BLEND
import org.lwjgl.opengl.GL11.GL_COLOR_BUFFER_BIT
import org.lwjgl.opengl.GL11.GL_DEPTH_TEST
import org.lwjgl.opengl.GL11.GL_LIGHTING
import org.lwjgl.opengl.GL11.GL_MODELVIEW
import org.lwjgl.opengl.GL11.GL_NEAREST
import org.lwjgl.opengl.GL11.GL_ONE_MINUS_SRC_ALPHA
import org.lwjgl.opengl.GL11.GL_PROJECTION
import org.lwjgl.opengl.GL11.GL_QUADS
import org.lwjgl.opengl.GL11.GL_SMOOTH
import org.lwjgl.opengl.GL11.GL_SRC_ALPHA
import org.lwjgl.opengl.GL11.GL_TEXTURE_2D
import org.lwjgl.opengl.GL11.GL_TEXTURE_MAG_FILTER
import org.lwjgl.opengl.GL11.glBegin
import org.lwjgl.opengl.GL11.glBlendFunc
import org.lwjgl.opengl.GL11.glClear
import org.lwjgl.opengl.GL11.glClearColor
import org.lwjgl.opengl.GL11.glClearDepth
import org.lwjgl.opengl.GL11.glDisable
import org.lwjgl.opengl.GL11.glEnable
import org.lwjgl.opengl.GL11.glEnd
import org.lwjgl.opengl.GL11.glLoadIdentity
import org.lwjgl.opengl.GL11.glMatrixMode
import org.lwjgl.opengl.GL11.glOrtho
import org.lwjgl.opengl.GL11.glShadeModel
import org.lwjgl.opengl.GL11.glTexCoord2f
import org.lwjgl.opengl.GL11.glTexParameteri
import org.lwjgl.opengl.GL11.glTranslatef
import org.lwjgl.opengl.GL11.glVertex2f
import org.lwjgl.opengl.GL11.glViewport
import org.newdawn.slick.Color
import org.newdawn.slick.TrueTypeFont
import org.newdawn.slick.opengl.Texture
import org.newdawn.slick.opengl.TextureLoader
import org.newdawn.slick.util.ResourceLoader

import Main.getConsumableList
import Main.getEpisode
import Main.getEquipmentList
import Main.getGameLog
import Main.getGameState
import Main.getGrid
import Main.getHeight
import Main.getLastWheel
import Main.getLevel
import Main.getMonsterList
import Main.getPassiveObjectList
import Main.getPlayer
import Main.getScrollList
import Main.getVersion
import Main.getWidth

/** Output is responsible for everything that is drawn on the screen.
 */
object Output {
  
  private val antiAlias: Boolean = true
  private val vSync: Boolean = false
  private val height: Int = getHeight
  private val width: Int = getWidth
  private val version: String = getVersion
  private var mouseScrollBonus: Int = 0
  private var mouseXCoord: Int = 0
  private var mouseYCoord: Int = 0
  
  var font: TrueTypeFont = null
  private var fontMenu: TrueTypeFont = null  
  private var awtFont: Font = null
  private var background:Texture = null
  private var characterBackground: Texture = null
  private var continue:Texture = null
  private var continue2:Texture = null
  private var newgame:Texture = null
  private var newgame2:Texture = null
  private var exit:Texture = null
  private var back:Texture = null
  private var unseen: Texture = null
  private var heart: Texture = null
  private var XP: Texture = null
  private var XPButton: Texture = null
  private var XPButton2: Texture = null
  private var quit: Texture = null
  private var HPBar: Texture = null
  private var XPBar: Texture = null
  private var zeal:Texture = null
  private var zeal2:Texture = null
  private var humility:Texture = null
  private var humility2:Texture = null
  private var temperance:Texture = null
  private var temperance2:Texture = null
  private var kindness:Texture = null
  private var kindness2:Texture = null
  private var patience:Texture = null
  private var patience2:Texture = null
  private var charity:Texture = null
  private var charity2:Texture = null
  private var diligence:Texture = null
  private var diligence2:Texture = null
  private var level:Texture = null
  private var level2: Texture = null
  private var tempUIBackground: Texture = null
  private var tempUIBackground2: Texture = null
  private var tempLevelBackground: Texture = null
  
  /** Prepares, launches and initializes the display.
   *
   * Starts by loading some crucial data from init-file.
   * Creates the display game uses for everything.
   * Prepares everything needed for the graphics library and drawing.
   * Loads and prepares fonts.
   * Loads images used in the game's mainmenu.
   */
  def startDisplay() {
    
    try {
      //for (mode <- Display.getAvailableDisplayModes) if (mode.getWidth == 1920 && mode.getHeight == 1080 && mode.getBitsPerPixel == 16 && mode.getFrequency == 60) Display.setDisplayMode(mode)
      //Display.setFullscreen(true)
      Display.setDisplayMode(new DisplayMode(width, height))
      Display.setTitle("Crusader" + " " + version)
      /** TODO
      // Window's game icon is not loaded right.
      //Display.setIcon(Array(ByteBuffer.allocate(0x4000).put(new ImageIOImageData().imageToByteBuffer(ImageIO.read(new File("data/icon.png")), false, false, null))))

      var bitit = Files.readAllBytes((new File("data/UI/icon.png")).toPath)
      var buffis = ByteBuffer.allocate(bitit.length).put(bitit)
      var buffis2 = new ImageIOImageData.imageToByteBuffer()
      var arr = (Array(buffis))
      Display.setIcon(arr)
      
      //var test = ByteBuffer.wrap(getBytesFromFile())
      //var test = new ImageIOImageData.imageToByteBuffer(ImageIO.read(new File("data/UI/icon.png")), false, false, null)

      Display.setIcon(new ByteBuffer {
        new Array(2)
        new ImageIOImageData
        new ImageIOImageData
      })
      */
      Display.create()
      Display.setVSyncEnabled(vSync)
    } catch {
      case e:LWJGLException =>
        e.printStackTrace()
        Display.destroy()
        System.exit(1)
    }

    glEnable(GL_TEXTURE_2D)
    glShadeModel(GL_SMOOTH)
    glDisable(GL_DEPTH_TEST)
    glDisable(GL_LIGHTING)
    glClearColor(0.0f, 0.0f, 0.0f, 0.0f)
    glClearDepth(1)
    glEnable(GL_BLEND)
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA)
    glViewport(0,0,width,height)
    
    glMatrixMode(GL_PROJECTION)
    glLoadIdentity()
    glOrtho(0, width, height, 0, 1, -1)
    glMatrixMode(GL_MODELVIEW)
    
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST)
    
    try {
      awtFont = Font.createFont(Font.TRUETYPE_FONT, 
          ResourceLoader.getResourceAsStream("data/UI/Kingthings Petrock.ttf"))
      awtFont = awtFont.deriveFont(70f)
      fontMenu = new TrueTypeFont(awtFont, antiAlias)
      awtFont = Font.createFont(Font.TRUETYPE_FONT, 
          ResourceLoader.getResourceAsStream("data/UI/Kingthings Petrock light.ttf"))
      awtFont = awtFont.deriveFont(24f)
      font = new TrueTypeFont(awtFont, antiAlias)
    } catch {
      case e: Throwable => e.printStackTrace()
    }
    
    org.lwjgl.input.Keyboard.enableRepeatEvents(true)
    
    background = loadTexture("tempBackground")
    characterBackground = loadTexture("tempCharacterBackground")
    continue = loadTexture("UI/Continue")
    continue2 = loadTexture("UI/Continue2")
    newgame = loadTexture("UI/Newgame")
    newgame2 = loadTexture("UI/Newgame2")
    exit = loadTexture("UI/Exit")
    back = loadTexture("UI/Back")
    unseen = loadTexture("Tiles/unseen")
    heart = loadTexture("UI/Heart")
    XP = loadTexture("UI/XP")
    XPButton = loadTexture("UI/XPButton")
    XPButton2 = loadTexture("UI/XPButton2")
    quit = loadTexture("UI/Quit")
    HPBar = loadTexture("UI/HPBar")
    XPBar = loadTexture("UI/XPBar")
    zeal = loadTexture("UI/Zeal")
    zeal2 = loadTexture("UI/Zeal2")
    humility = loadTexture("UI/Humility")
    humility2 = loadTexture("UI/Humility2")
    temperance = loadTexture("UI/temperance")
    temperance2 = loadTexture("UI/temperance2")
    kindness = loadTexture("UI/kindness")
    kindness2 = loadTexture("UI/kindness2")
    patience = loadTexture("UI/patience")
    patience2 = loadTexture("UI/patience2")
    charity = loadTexture("UI/charity")
    charity2 = loadTexture("UI/charity2")
    diligence = loadTexture("UI/diligence")
    diligence2 = loadTexture("UI/diligence2")
    level = loadTexture("UI/Level")
    level2 = loadTexture("UI/Level2")
    tempUIBackground = loadTexture("tempUIBackground")
    tempUIBackground2 = loadTexture("tempUIBackground2")
    tempLevelBackground = loadTexture("tempLevelBackground")
    
    addLog("Insert epic story here. (TODO)")
    
  }
  
  /** Method to load textures from given location.
   *  
   *  Since all images are in png format and in the data folder, it is set already.
   *
   * @param path is location of the texture
   * @return a Texture build from image at the given location
   */
  def loadTexture(path: String): Texture = {
    var tex:Texture = null
    val in:InputStream = ResourceLoader.getResourceAsStream("data/" + path + ".png")
    try {
      tex = TextureLoader.getTexture("PNG", in)
    } catch {
      case e:IOException => e.printStackTrace()
      tex = TextureLoader.getTexture("PNG", ResourceLoader.getResourceAsStream("data/UI/missing.png"))
    }
    tex
  }
  
  /** Method to draw textures on the display.
   *
   * @param tex is the texture used
   * @param x coordinate for the quad
   * @param y coordinate for the quad
   * @param width for the quad
   * @param height for the quad
   */
  def drawQuadTex(tex: Texture, x: Float, y: Float, width: Float, height: Float) {
    Color.white.bind
    tex.bind
    glTranslatef(x, y, 0)
    glBegin(GL_QUADS)
    glTexCoord2f(0, 0)
    glVertex2f(0, 0)
    glTexCoord2f(1, 0)
    glVertex2f(width, 0)
    glTexCoord2f(1, 1)
    glVertex2f(width, height)
    glTexCoord2f(0, 1)
    glVertex2f(0, height)
    glEnd
    glLoadIdentity
  }
  
  /** Draw the character creation */
  def drawCharacterCreation() {
    glClear(GL_COLOR_BUFFER_BIT)
    drawQuadTex(characterBackground, (width-characterBackground.getImageWidth)/2, 
        (height-characterBackground.getImageHeight)/2, characterBackground.getImageWidth, characterBackground.getImageHeight)
    if (font.getWidth(getPlayer.name) >= 400) {
      font.drawString(width/2-font.getWidth("Name is too long")/2, height*5/9, "Name is too long", Color.red)
      drawQuadTex(newgame2, width*23/24-newgame2.getImageWidth+54, height*7/10, newgame2.getImageWidth, newgame2.getImageHeight)
    }
    else drawQuadTex(newgame, width*5/8, height*7/10, newgame.getImageWidth, newgame.getImageHeight)
    drawQuadTex(back, width*3/8-back.getImageWidth+34, height*7/10, back.getImageWidth, back.getImageHeight)
    fontMenu.drawString(width/2-fontMenu.getWidth("Enter name:")/2, height*3/9, "Enter name:", Color.black)
    font.drawString(width/2-font.getWidth(getPlayer.name)/2, height*4/9, getPlayer.name, Color.black)
    font.drawString(2, height - 24, "Mouse X: " + Mouse.getX.toString, Color.red)
    font.drawString(130, height - 24, "Y: " + (height - Mouse.getY).toString, Color.red)
  }
  
  /** Draw the main menu */
  def drawMainMenu() {
    glClear(GL_COLOR_BUFFER_BIT)
    drawQuadTex(background, (width-background.getImageWidth)/2, 
        (height-background.getImageHeight)/2, background.getImageWidth, background.getImageHeight)
    drawQuadTex(continue2, width*17/24, height*1/6, continue.getImageWidth, continue.getImageHeight)
    drawQuadTex(newgame, width*17/24, height*1/6+100, newgame.getImageWidth, newgame.getImageHeight)
    drawQuadTex(exit, width*17/24, height*1/6+200, exit.getImageWidth, exit.getImageHeight)
    fontMenu.drawString(920, height/10, "Crusader", Color.black)        
    font.drawString(2, height - 24, "Mouse X: " + Mouse.getX.toString, Color.red)
    font.drawString(130, height - 24, "Y: " + (height - Mouse.getY).toString, Color.red)
  }
  
  /** Draw game screen */
  def drawGame() {
    mouseXCoord = (Mouse.getX * 1.0 / 32 + getPlayer.getX - 16).toInt
    mouseYCoord = ((height - Mouse.getY) * 1.0 / 32 + getPlayer.getY - 8).toInt
    glClear(GL_COLOR_BUFFER_BIT)
    drawSideBar
    drawlog
    drawFloor
    drawPlayer
    drawObjects
    drawFog
    font.drawString(2, height - 245, "Mouse X: " + mouseXCoord.toString, Color.red)
    font.drawString(130, height - 245, "Y: " + mouseYCoord.toString, Color.red)
    font.drawString(2, height - 225, "Mouse X: " + Mouse.getX.toString, Color.red)
    font.drawString(130, height - 225, "Y: " + (height - Mouse.getY).toString, Color.red)
    font.drawString(2, height - 205, "Version: " + version, Color.white)
    font.drawString(900, height - 205, "Episode: " + getEpisode, Color.white)
    font.drawString(990, height - 205, "Level: " + getLevel, Color.white)
    if (Mouse.getX < 1057 && (height - Mouse.getY) < 545) drawInfoBox
    if (getGameState == GameState.LEVEL) drawLevel
  }
  
  /** Method to wrap long strings into smaller lines.
   *
   * @param text is the long text block needed to split
   * @param maxLenght is the maximum lenght (in pixels) for one line
   * @return Buffer containing smaller strings
   */
  def wordWrap(text: String, maxLength: Int): Buffer[String] = {
    val lines = Buffer[String]()
    var line = ""
    for (word <- text.split(" ")) {
      if (Output.font.getWidth(line + " " + word) > maxLength) {
        lines += line
        line = word + " "
      }
      else line += word + " "
    }
    lines += line
    lines
  }
  
  /** Add message to gamelogs */
  def addLog(text: String) = {
    for (line <- wordWrap(text, 1030)) {
      getGameLog.append(line)
      mouseScrollBonus += 1
    }
  }
  
  /** Draw gamelog to bottom of the screen */
  def drawlog() {
    getLastWheel match {
      case w if (w > 0) => mouseScrollBonus -= 1
      case w if (w < 0) => mouseScrollBonus += 1
      case _ =>
    }
    if (mouseScrollBonus < -getGameLog.size+7) mouseScrollBonus = -getGameLog.size+7
    else if (mouseScrollBonus > 0) mouseScrollBonus = 0
    
    var tempLog = Buffer[String]()
    if (getGameLog.size <= 7) tempLog = getGameLog
    else for (line <- Range(getGameLog.size - 7 + mouseScrollBonus, getGameLog.size + mouseScrollBonus)) tempLog.append(getGameLog.apply(line))
    var num: Int = 0
    for (line <- tempLog.reverse) {
      num += 1
      font.drawString(10, height - 20 - num * 20, line, Color.black)
    }
  }
  
  /** Draw sidebar to right of the screen */
  def drawSideBar() {
    val middle = 33*32 + (width - 33*32)/2
    var h = 0
    
    drawQuadTex(tempUIBackground2, (width-tempUIBackground2.getImageWidth)/2, 
        (height-tempUIBackground2.getImageHeight)/2, tempUIBackground2.getImageWidth, tempUIBackground2.getImageHeight)
    
    var HPHeight = getPlayer.health / getPlayer.maxHealth
    if (HPHeight > 1) HPHeight = 1
    if (HPHeight < 0) HPHeight = 0
    drawQuadTex(HPBar, middle-heart.getImageWidth/2+52, 46 + ((heart.getImageHeight-140) * (1-HPHeight)).toInt, 
        heart.getImageWidth-104, ((heart.getImageHeight-140) * HPHeight).toInt)
    
    var XPWidth = getPlayer.experience*1.0 / getPlayer.smallestLevel
    if (XPWidth > 1) XPWidth = 1
    if (XPWidth < 0) XPWidth = 0
    drawQuadTex(XPBar, middle-XP.getImageWidth/2+40, 186, 
        ((XP.getImageWidth-80)*XPWidth).toInt, XP.getImageHeight-134)
    
    drawQuadTex(tempUIBackground, (width-tempUIBackground.getImageWidth)/2, 
        (height-tempUIBackground.getImageHeight)/2, tempUIBackground.getImageWidth, tempUIBackground.getImageHeight)
    
    h -= 25
    drawQuadTex(heart, middle-heart.getImageWidth/2, h, heart.getImageWidth, heart.getImageHeight)
    
    h += 115
    val hptext = "HP: " + getPlayer.health.toInt + "/" + getPlayer.maxHealth
    font.drawString(middle-font.getWidth(hptext)/2, h, hptext, Color.black)
    
    h -= 16
    drawQuadTex(XP, middle-XP.getImageWidth/2, h, XP.getImageWidth, XP.getImageHeight)
    
    h += 112
    val xptext = "XP: " + getPlayer.experience.toInt + "/" + getPlayer.smallestLevel
    font.drawString(middle-font.getWidth(xptext)/2, h, xptext, Color.black)
    
    if (getPlayer.experience >= getPlayer.smallestLevel) 
      drawQuadTex(XPButton, middle-XPButton.getImageWidth/2, 194-48, XPButton.getImageWidth, 
          XPButton.getImageHeight)
    else drawQuadTex(XPButton2, middle-XPButton2.getImageWidth/2, 194-48, XPButton2.getImageWidth, 
        XPButton2.getImageHeight)
        
    h += 150
    if (getPlayer.slotWeapon != null) drawQuadTex(getPlayer.slotWeapon.image, 
        middle-getPlayer.slotWeapon.image.getImageWidth*5/2, h, 
        getPlayer.slotWeapon.image.getImageWidth, getPlayer.slotWeapon.image.getImageHeight)
    if (getPlayer.slotArmor != null) drawQuadTex(getPlayer.slotArmor.image, 
        middle-getPlayer.slotArmor.image.getImageWidth/2, h, 
        getPlayer.slotArmor.image.getImageWidth, getPlayer.slotArmor.image.getImageHeight)
    if (getPlayer.slotShield != null) drawQuadTex(getPlayer.slotShield.image, 
        middle+getPlayer.slotShield.image.getImageWidth*3/2, h, 
        getPlayer.slotShield.image.getImageWidth, getPlayer.slotShield.image.getImageHeight)
    
    h += 64
    if (getPlayer.slotRing != null) drawQuadTex(getPlayer.slotRing.image, 
        middle-getPlayer.slotRing.image.getImageWidth*5/2, h, 
        getPlayer.slotRing.image.getImageWidth, getPlayer.slotRing.image.getImageHeight)
    if (getPlayer.slotAmulet != null) drawQuadTex(getPlayer.slotAmulet.image, 
        middle-getPlayer.slotAmulet.image.getImageWidth/2, h, 
        getPlayer.slotAmulet.image.getImageWidth, getPlayer.slotAmulet.image.getImageHeight)
    if (getPlayer.slotItem != null) drawQuadTex(getPlayer.slotItem.image, 
        middle+getPlayer.slotItem.image.getImageWidth*3/2, h, 
        getPlayer.slotItem.image.getImageWidth, getPlayer.slotItem.image.getImageHeight)
    
    h += 64
    val name = "Name: " + getPlayer.name
    font.drawString(middle-96, h, name, Color.black)
    
    h += 20
    val title = "Title: " + getPlayer.title
    font.drawString(middle-96, h, title, Color.black)
    
    h += 20
    val heroLevel = "Hero level: " + getPlayer.totalLevel
    font.drawString(middle-96, h, heroLevel, Color.black)
    
    h += 20
    val gold = "Gold: " + getPlayer.gold.toInt
    font.drawString(middle-96, h, gold, Color.black)
    
    h += 20
    val piety = "Piety: " + getPlayer.piety.toInt
    font.drawString(middle-96, h, piety, Color.black)
    
    drawQuadTex(quit, middle-quit.getImageWidth/2, h, quit.getImageWidth, quit.getImageHeight)
  }
  
  /** Draw player to the middle of screen */
  def drawPlayer() = getPlayer.draw
  
  /** Draws Tiles */
  def drawFloor() {
    for (i <- Range(0,33)) {
      for (j <- Range(0,17)) {
        drawQuadTex(unseen, i * 32, j * 32, unseen.getImageWidth, unseen.getImageHeight)
      }
    }
    getGrid.draw
  }
  
  /** Draw objects */
  def drawObjects() {
    for (list <- List(getEquipmentList, getScrollList, getConsumableList, getMonsterList, getPassiveObjectList)) {
      for (obj <- list) {
        if (obj.xDif(getPlayer) <= 16 && obj.yDif(getPlayer) <= 8 && 
            getGrid.getTile(obj.getX, obj.getY).explored) obj.draw
      }
    }
  }
  
  /** Draw fog */
  def drawFog() {
    getGrid.FOV
    for (i <- Range(0, getGrid.getSize())) {
      for (j <- Range(0, getGrid.getSize())) {
        if (getPlayer.xDif(i) <= 16 && getPlayer.yDif(j) <= 8) getGrid.getTile(i, j).drawFog
      }
    }
  }
  
  /** Draw info box about objects under mouse */
  def drawInfoBox() {
    // TODO
    //for (obj <- getGrid.getTile(mouseXCoord, mouseYCoord).getObjectList) println(obj.name)
  }
  
  /** Draw the character level up screen */
  def drawLevel {
    drawQuadTex(tempLevelBackground, (width-tempLevelBackground.getImageWidth)/2, 
        (height-tempLevelBackground.getImageHeight)/2, tempLevelBackground.getImageWidth, 
        tempLevelBackground.getImageHeight)
    val title = "Choose level:"
    var h = 20
    var v = 150
    fontMenu.drawString(v, 35, title, Color.black)
    if (getPlayer.experience >= getPlayer.xpNeededForLevel(getPlayer.zeal)) 
      drawQuadTex(zeal, v, h, zeal.getImageWidth, zeal.getImageHeight)
    else drawQuadTex(zeal2, v, h, zeal2.getImageWidth, zeal2.getImageHeight)
    h += 70
    if (getPlayer.experience >= getPlayer.xpNeededForLevel(getPlayer.humility)) 
      drawQuadTex(humility, v, h, humility.getImageWidth, humility.getImageHeight)
    else drawQuadTex(humility2, v, h, humility2.getImageWidth, humility2.getImageHeight)
    h += 70
    if (getPlayer.experience >= getPlayer.xpNeededForLevel(getPlayer.temperance)) 
      drawQuadTex(temperance, v, h, temperance.getImageWidth, temperance.getImageHeight)
    else drawQuadTex(temperance2, v, h, temperance2.getImageWidth, temperance2.getImageHeight)
    h += 70
    if (getPlayer.experience >= getPlayer.xpNeededForLevel(getPlayer.kindness)) 
      drawQuadTex(kindness, v, h, kindness.getImageWidth, kindness.getImageHeight)
    else drawQuadTex(kindness2, v, h, kindness2.getImageWidth, kindness2.getImageHeight)
    h += 70
    if (getPlayer.experience >= getPlayer.xpNeededForLevel(getPlayer.patience)) 
      drawQuadTex(patience, v, h, patience.getImageWidth, patience.getImageHeight)
    else drawQuadTex(patience2, v, h, patience2.getImageWidth, patience2.getImageHeight)
    h += 70
    if (getPlayer.experience >= getPlayer.xpNeededForLevel(getPlayer.charity)) 
      drawQuadTex(charity, v, h, charity.getImageWidth, charity.getImageHeight)
    else drawQuadTex(charity2, v, h, charity2.getImageWidth, charity2.getImageHeight)
    h += 70
    if (getPlayer.experience >= getPlayer.xpNeededForLevel(getPlayer.diligence)) 
      drawQuadTex(diligence, v, h, diligence.getImageWidth, diligence.getImageHeight)
    else drawQuadTex(diligence2, v, h, diligence2.getImageWidth, diligence2.getImageHeight)
    h += 70
    drawQuadTex(back, v, h, back.getImageWidth, back.getImageHeight)
    
    h = 128
    v = 400
    
    for (lev <- Range(0,10)) {
      v += 16
      if (lev + 1 <= getPlayer.zeal) drawQuadTex(level, v, h, level.getImageWidth, level.getImageHeight)
      else drawQuadTex(level2, v, h, level2.getImageWidth, level2.getImageHeight)
    }
    v += 32
    font.drawString(v, h, "Costs: " + getPlayer.xpNeededForLevel(getPlayer.zeal), Color.black)
    v += 128
    font.drawString(v, h, "Short description of skill (TODO)", Color.black)
    h += 70
    v = 400
    for (lev <- Range(0,10)) {
      v += 16
      if (lev + 1 <= getPlayer.humility) drawQuadTex(level, v, h, level.getImageWidth, level.getImageHeight)
      else drawQuadTex(level2, v, h, level2.getImageWidth, level2.getImageHeight)
    }
    v += 32
    font.drawString(v, h, "Costs: " + getPlayer.xpNeededForLevel(getPlayer.humility), Color.black)
    v += 128
    font.drawString(v, h, "Short description of skill (TODO)", Color.black)
    h += 70
    v = 400
    for (lev <- Range(0,10)) {
      v += 16
      if (lev + 1 <= getPlayer.temperance) drawQuadTex(level, v, h, level.getImageWidth, level.getImageHeight)
      else drawQuadTex(level2, v, h, level2.getImageWidth, level2.getImageHeight)
    }
    v += 32
    font.drawString(v, h, "Costs: " + getPlayer.xpNeededForLevel(getPlayer.temperance), Color.black)
    v += 128
    font.drawString(v, h, "Short description of skill (TODO)", Color.black)
    h += 70
    v = 400
    for (lev <- Range(0,10)) {
      v += 16
      if (lev + 1 <= getPlayer.kindness) drawQuadTex(level, v, h, level.getImageWidth, level.getImageHeight)
      else drawQuadTex(level2, v, h, level2.getImageWidth, level2.getImageHeight)
    }
    v += 32
    font.drawString(v, h, "Costs: " + getPlayer.xpNeededForLevel(getPlayer.kindness), Color.black)
    v += 128
    font.drawString(v, h, "Short description of skill (TODO)", Color.black)
    h += 70
    v = 400
    for (lev <- Range(0,10)) {
      v += 16
      if (lev + 1 <= getPlayer.patience) drawQuadTex(level, v, h, level.getImageWidth, level.getImageHeight)
      else drawQuadTex(level2, v, h, level2.getImageWidth, level2.getImageHeight)
    }
    v += 32
    font.drawString(v, h, "Costs: " + getPlayer.xpNeededForLevel(getPlayer.patience), Color.black)
    v += 128
    font.drawString(v, h, "Short description of skill (TODO)", Color.black)
    h += 70
    v = 400
    for (lev <- Range(0,10)) {
      v += 16
      if (lev + 1 <= getPlayer.charity) drawQuadTex(level, v, h, level.getImageWidth, level.getImageHeight)
      else drawQuadTex(level2, v, h, level2.getImageWidth, level2.getImageHeight)
    }
    v += 32
    font.drawString(v, h, "Costs: " + getPlayer.xpNeededForLevel(getPlayer.charity), Color.black)
    v += 128
    font.drawString(v, h, "Short description of skill (TODO)", Color.black)
    h += 70
    v = 400
    for (lev <- Range(0,10)) {
      v += 16
      if (lev + 1 <= getPlayer.diligence) drawQuadTex(level, v, h, level.getImageWidth, level.getImageHeight)
      else drawQuadTex(level2, v, h, level2.getImageWidth, level2.getImageHeight)
    }
    v += 32
    font.drawString(v, h, "Costs: " + getPlayer.xpNeededForLevel(getPlayer.diligence), Color.black)
    v += 128
    font.drawString(v, h, "Short description of skill (TODO)", Color.black)
    
  }
  
}