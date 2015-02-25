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
  private var tempHeart: Texture = null
  private var tempXp: Texture = null
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
    tempHeart = loadTexture("tempHeart")
    tempXp = loadTexture("tempXp")
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
    else drawQuadTex(newgame, width*23/24-newgame.getImageWidth+54, height*7/10, newgame.getImageWidth, newgame.getImageHeight)
    drawQuadTex(back, 1/24, height*7/10, back.getImageWidth, back.getImageHeight)
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
    drawQuadTex(tempUIBackground, 0, 0, tempUIBackground.getImageWidth, tempUIBackground.getImageHeight)
    drawlog
    drawSideBar
    drawFloor
    drawObjects
    drawPlayer
    drawFog
    font.drawString(2, 0, getPlayer.name + " the Holy", Color.white)
    font.drawString(width - 155, height - 27, "Esc to quit", Color.black)
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
    drawQuadTex(tempHeart, width-239, -25, tempHeart.getImageWidth, tempHeart.getImageHeight)
    font.drawString(width-153, 80, "HP: " + getPlayer.health.toInt + "/" + getPlayer.maxHealth, Color.black)
    drawQuadTex(tempXp, width-239, 100, tempXp.getImageWidth, tempXp.getImageHeight)
    font.drawString(width-153, 210, "XP: " + getPlayer.experience + "/10", Color.black)
    font.drawString(width-200, 410, "Items and stuff here", Color.black)
    font.drawString(width-200, 410, "Items and stuff here", Color.black)
    if (getPlayer.slotWeapon != null) drawQuadTex(getPlayer.slotWeapon.image, width-200, 500, getPlayer.slotWeapon.image.getImageWidth, getPlayer.slotWeapon.image.getImageHeight)
    if (getPlayer.slotArmor != null) drawQuadTex(getPlayer.slotArmor.image, width-150, 500, getPlayer.slotArmor.image.getImageWidth, getPlayer.slotArmor.image.getImageHeight)
    if (getPlayer.slotShield != null) drawQuadTex(getPlayer.slotShield.image, width-100, 500, getPlayer.slotShield.image.getImageWidth, getPlayer.slotShield.image.getImageHeight)
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