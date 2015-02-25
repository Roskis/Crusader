package game

import org.lwjgl._
import org.lwjgl.LWJGLException
import org.lwjgl.opengl.{Display, DisplayMode, GL11}
import org.lwjgl.opengl.GL11._
import org.lwjgl.input.Mouse
import org.newdawn.slick.opengl.{Texture, TextureLoader}
import org.newdawn.slick.{Color, TrueTypeFont}
import org.newdawn.slick.util.ResourceLoader
import scala.io.Source
import collection.mutable.Buffer
import java.io.{IOException, InputStream, File}
import java.awt.Font
import Main._

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
  private var saveQuit: Texture = null
  private var HPBar: Texture = null
  private var XPBar: Texture = null
  private var tempUIBackground: Texture = null
  
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
      // Window's game icon is not loaded right. TODO
      //Display.setIcon(Array(ByteBuffer.allocate(0x4000).put(new ImageIOImageData().imageToByteBuffer(ImageIO.read(new File("data/icon.png")), false, false, null))))
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
    saveQuit = loadTexture("UI/Save&Quit")
    HPBar = loadTexture("UI/HPBar")
    XPBar = loadTexture("UI/XPBar")
    tempUIBackground = loadTexture("tempUIBackground")
    
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
    drawQuadTex(tempUIBackground, (width-tempUIBackground.getImageWidth)/2, 
        (height-tempUIBackground.getImageHeight)/2, tempUIBackground.getImageWidth, tempUIBackground.getImageHeight)
    drawSideBar
    drawlog
    drawFloor
    drawObjects
    drawPlayer
    drawFog
    font.drawString(2, height - 245, "Mouse X: " + mouseXCoord.toString, Color.red)
    font.drawString(130, height - 245, "Y: " + mouseYCoord.toString, Color.red)
    font.drawString(2, height - 225, "Mouse X: " + Mouse.getX.toString, Color.red)
    font.drawString(130, height - 225, "Y: " + (height - Mouse.getY).toString, Color.red)
    font.drawString(2, height - 205, "Version: " + version, Color.white)
    font.drawString(900, height - 205, "Episode: " + getEpisode, Color.white)
    font.drawString(990, height - 205, "Level: " + getLevel, Color.white)
    if (Mouse.getX < 1057 && (height - Mouse.getY) < 545) drawInfoBox
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
    var height = 0
    
    var HPHeight = getPlayer.health / getPlayer.maxHealth
    if (HPHeight > 1) HPHeight = 1
    if (HPHeight < 0) HPHeight = 0
    drawQuadTex(HPBar, middle-heart.getImageWidth/2+52, (height+46) + ((heart.getImageHeight-140) * (1-HPHeight)).toInt, 
        heart.getImageWidth-104, ((heart.getImageHeight-140) * HPHeight).toInt)
    
    height -= 25
    drawQuadTex(heart, middle-heart.getImageWidth/2, height, heart.getImageWidth, heart.getImageHeight)
    
    height += 115
    val hptext = "HP: " + getPlayer.health.toInt + "/" + getPlayer.maxHealth
    font.drawString(middle-font.getWidth(hptext)/2, height, hptext, Color.black)
    
    var XPWidth = getPlayer.experience*1.0 / getPlayer.smallestLevel
    if (XPWidth > 1) XPWidth = 1
    if (XPWidth < 0) XPWidth = 0
    drawQuadTex(XPBar, middle-XP.getImageWidth/2+40, height+96, 
        ((XP.getImageWidth-80)*XPWidth).toInt, XP.getImageHeight-224)
    
    height -= 16
    drawQuadTex(XP, middle-XP.getImageWidth/2, height, XP.getImageWidth, XP.getImageHeight)
    
    height += 112
    val xptext = "XP: " + getPlayer.experience + "/" + getPlayer.smallestLevel
    font.drawString(middle-font.getWidth(xptext)/2, height, xptext, Color.black)
    if (getPlayer.experience > getPlayer.smallestLevel) 
      drawQuadTex(XPButton, middle-XPButton.getImageWidth/2, 194-48, XPButton.getImageWidth, 
          XPButton.getImageHeight)
    else drawQuadTex(XPButton2, middle-XPButton2.getImageWidth/2, 194-48, XPButton2.getImageWidth, 
        XPButton2.getImageHeight)
        
    height += 150
    if (getPlayer.slotWeapon != null) drawQuadTex(getPlayer.slotWeapon.image, 
        middle-getPlayer.slotWeapon.image.getImageWidth*5/2, height, 
        getPlayer.slotWeapon.image.getImageWidth, getPlayer.slotWeapon.image.getImageHeight)
    if (getPlayer.slotArmor != null) drawQuadTex(getPlayer.slotArmor.image, 
        middle-getPlayer.slotArmor.image.getImageWidth/2, height, 
        getPlayer.slotArmor.image.getImageWidth, getPlayer.slotArmor.image.getImageHeight)
    if (getPlayer.slotShield != null) drawQuadTex(getPlayer.slotShield.image, 
        middle+getPlayer.slotShield.image.getImageWidth*3/2, height, 
        getPlayer.slotShield.image.getImageWidth, getPlayer.slotShield.image.getImageHeight)
    
    height += 64
    if (getPlayer.slotRing != null) drawQuadTex(getPlayer.slotRing.image, 
        middle-getPlayer.slotRing.image.getImageWidth*5/2, height, 
        getPlayer.slotRing.image.getImageWidth, getPlayer.slotRing.image.getImageHeight)
    if (getPlayer.slotAmulet != null) drawQuadTex(getPlayer.slotAmulet.image, 
        middle-getPlayer.slotAmulet.image.getImageWidth/2, height, 
        getPlayer.slotAmulet.image.getImageWidth, getPlayer.slotAmulet.image.getImageHeight)
    if (getPlayer.slotItem != null) drawQuadTex(getPlayer.slotItem.image, 
        middle+getPlayer.slotItem.image.getImageWidth*3/2, height, 
        getPlayer.slotItem.image.getImageWidth, getPlayer.slotItem.image.getImageHeight)
    
    height += 64
    val name = "Name: " + getPlayer.name
    font.drawString(middle-96, height, name, Color.black)
    
    height += 20
    val title = "Title: " + getPlayer.title
    font.drawString(middle-96, height, title, Color.black)
    
    height += 20
    val heroLevel = "Hero level: " + getPlayer.totalLevel
    font.drawString(middle-96, height, heroLevel, Color.black)
    
    height += 20
    val gold = "Gold: " + getPlayer.gold
    font.drawString(middle-96, height, gold, Color.black)
    
    height += 20
    val piety = "Piety: " + getPlayer.piety
    font.drawString(middle-96, height, piety, Color.black)
    
    height -= 64
    drawQuadTex(saveQuit, middle-saveQuit.getImageWidth/2, height, saveQuit.getImageWidth, saveQuit.getImageHeight)
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
    for (list <- List(getPassiveObjectList, getEquipmentList, getScrollList, getConsumableList, getMonsterList)) {
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
    //for (obj <- getGrid.getTile(mouseXCoord, mouseYCoord).getObjectList) println(obj.name)
  }
  
}