package game

import java.awt.Font
import java.io.IOException
import java.io.File

import scala.Range
import scala.collection.mutable.Buffer

import org.lwjgl.LWJGLException
import org.lwjgl.input.Mouse
import org.lwjgl.opengl.{Display, DisplayMode}
import org.lwjgl.opengl.GL11._
import org.newdawn.slick.{Color, TrueTypeFont}
import org.newdawn.slick.opengl.{Texture}
import org.newdawn.slick.util.ResourceLoader

import Main._
import Helpers._

/** Output is responsible for everything that is drawn on the screen.
 */
object Output {
  
  private val antiAlias: Boolean = true
  private val vSync: Boolean = false
  private var mouseXCoord: Int = 0
  private var mouseYCoord: Int = 0
  
  var mouseScrollBonus: Int = 0
  var font: TrueTypeFont = null
  private var fontMenu: TrueTypeFont = null  
  private var awtFont: Font = null
  
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
      Display.setFullscreen(getFullscreen)
      Display.setDisplayMode(getDisplayMode)
      Display.setTitle("Crusader" + " " + getVersion)
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
    glViewport(0,0,getWidth,getHeight)
    
    glMatrixMode(GL_PROJECTION)
    glLoadIdentity()
    glOrtho(0, getWidth, getHeight, 0, 1, -1)
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
    
    buttonContinue = new Button(905, 200, 256, 64, continue, continue2)
    buttonNewGameMenu = new Button(905, 280, 256, 64, newgame, newgame2)
    buttonOptions = new Button(905, 360, 256, 64, options, options)
    buttonCredits = new Button(905, 440, 256, 64, credits, credits)
    buttonExit = new Button(905, 520, 256, 64, exit, exit)
    buttonNewGameChar = new Button(799, 600, 256, 64, newgame, newgame2)
    buttonBackChar = new Button(257, 600, 256, 64, back, back)
    buttonXP = new Button(1070, 242, 200, 64, XPButton, XPButton2)
    buttonQuit = new Button(1070, 640, 200, 64, quit, quit)
    buttonBackLVL = new Button(149, 606, 256, 64, back, back)
    buttonCharity = new Button(149, 117, 256, 64, charity, charity2)
    buttonDiligence = new Button(149, 187, 256, 64, diligence, diligence2)
    buttonHumility = new Button(149, 257, 256, 64, humility, humility2)
    buttonKindness = new Button(149, 327, 256, 64, kindness, kindness2)
    buttonPatience = new Button(149, 397, 256, 64, patience, patience2)
    buttonTemperance = new Button(149, 467, 256, 64, temperance, temperance2)
    buttonZeal = new Button(149, 537, 256, 64, zeal, zeal2)
    
    org.lwjgl.input.Keyboard.enableRepeatEvents(true)
    addLog("Your holy mission is to purify the world from evil.")
    
  }
  
  /** Method is called when we want to change the resolution. */
  def changeResolution() = {
    Display.setDisplayMode(getDisplayMode)
    glViewport(0,0,getDisplayMode.getWidth,getDisplayMode.getHeight)
    Display.setFullscreen(getFullscreen)
    
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
    drawQuadTex(emptyBackground, (getWidth-emptyBackground.getImageWidth)/2, 
        (getHeight-emptyBackground.getImageHeight)/2, emptyBackground.getImageWidth, emptyBackground.getImageHeight)
    if (font.getWidth(getPlayer.name) >= 200) {
      font.drawString(getWidth/2-font.getWidth("Name is too long")/2, getHeight*5/9, "Name is too long", Color.red)
      drawQuadTex(buttonNewGameChar.tex2, buttonNewGameChar.getDrawX, buttonNewGameChar.getDrawY, buttonNewGameChar.tex2.getImageWidth, buttonNewGameChar.tex2.getImageHeight)
    }
    else drawQuadTex(buttonNewGameChar.tex, buttonNewGameChar.getDrawX, buttonNewGameChar.getDrawY, buttonNewGameChar.tex.getImageWidth, buttonNewGameChar.tex.getImageHeight)
    drawQuadTex(buttonBackChar.tex, buttonBackChar.getDrawX, buttonBackChar.getDrawY, buttonBackChar.tex.getImageWidth, buttonBackChar.tex.getImageHeight)
    fontMenu.drawString(getWidth/2-fontMenu.getWidth("Enter name:")/2, getHeight*3/9, "Enter name:", Color.black)
    font.drawString(getWidth/2-font.getWidth(getPlayer.name)/2, getHeight*4/9, getPlayer.name, Color.black)
    font.drawString(2, getHeight - 24, "Mouse X: " + Mouse.getX.toString, Color.red)
    font.drawString(130, getHeight - 24, "Y: " + (getHeight - Mouse.getY).toString, Color.red)
  }
  
  /** Draw options */
  def drawOptions() {
    glClear(GL_COLOR_BUFFER_BIT)
    drawQuadTex(emptyBackground, (getWidth-emptyBackground.getImageWidth)/2, 
        (getHeight-emptyBackground.getImageHeight)/2, emptyBackground.getImageWidth, emptyBackground.getImageHeight)
    val text = "Resolution: " + getDisplayMode.getWidth + "x" + getDisplayMode.getHeight
    font.drawString(getWidth/2-font.getWidth(text)/2, getHeight/3, text, Color.black)
    val text1 = "Up and down to change resolution."
    font.drawString(getWidth/2-font.getWidth(text1)/2, getHeight/3+25, text1, Color.black)
    val text2 = "ALT+ENTER to change to fullscreen."
    font.drawString(getWidth/2-font.getWidth(text2)/2, getHeight/3+50, text2, Color.black)
    val text3 = "Escape to go back."
    font.drawString(getWidth/2-font.getWidth(text3)/2, getHeight/3+75, text3, Color.black)
    val text4 = "Recommended resolution: 1280x720."
    font.drawString(getWidth/2-font.getWidth(text4)/2, getHeight/3+100, text4, Color.black)
  }
  
  /** Draw the main menu */
  def drawMainMenu() {
    glClear(GL_COLOR_BUFFER_BIT)
    drawQuadTex(background, (getWidth-background.getImageWidth)/2, 
        (getHeight-background.getImageHeight)/2, background.getImageWidth, background.getImageHeight)
    if (new File("save.dat").exists) drawQuadTex(buttonContinue.tex, buttonContinue.getDrawX.toInt, 
        buttonContinue.getDrawY.toInt, buttonContinue.tex.getImageWidth, buttonContinue.tex.getImageHeight)
    else drawQuadTex(buttonContinue.tex2, buttonContinue.getDrawX, buttonContinue.getDrawY, 
        buttonContinue.tex2.getImageWidth, buttonContinue.tex2.getImageHeight)
    drawQuadTex(buttonNewGameMenu.tex, buttonNewGameMenu.getDrawX, buttonNewGameMenu.getDrawY, 
        buttonNewGameMenu.tex.getImageWidth, buttonNewGameMenu.tex.getImageHeight)
    drawQuadTex(buttonOptions.tex, buttonOptions.getDrawX, buttonOptions.getDrawY, 
        buttonOptions.tex.getImageWidth, buttonOptions.tex.getImageHeight)
    drawQuadTex(buttonCredits.tex, buttonCredits.getDrawX, buttonCredits.getDrawY, 
        buttonCredits.tex.getImageWidth, buttonCredits.tex.getImageHeight)
    drawQuadTex(buttonExit.tex, buttonExit.getDrawX, buttonExit.getDrawY, 
        buttonExit.tex.getImageWidth, buttonExit.tex.getImageHeight)
    fontMenu.drawString(920, getHeight/10, "Crusader", Color.black)        
    font.drawString(2, getHeight - 24, "Mouse X: " + Mouse.getX.toString, Color.red)
    font.drawString(130, getHeight - 24, "Y: " + (getHeight - Mouse.getY).toString, Color.red)
  }
  
  /** Draw game screen */
  def drawGame() {
    mouseXCoord = (Mouse.getX * 1.0 / 32 + getPlayer.getX - 16).toInt
    mouseYCoord = ((getHeight - Mouse.getY) * 1.0 / 32 + getPlayer.getY - 8).toInt
    glClear(GL_COLOR_BUFFER_BIT)
    drawQuadTex(tempUIBackground2, (getWidth-tempUIBackground2.getImageWidth)/2, 
        (getHeight-tempUIBackground2.getImageHeight)/2, tempUIBackground2.getImageWidth, tempUIBackground2.getImageHeight)
    drawTiles
    drawSideBar
    drawLog
    drawFog
    font.drawString(2, getHeight - 245, "Mouse X: " + mouseXCoord.toString, Color.red)
    font.drawString(130, getHeight - 245, "Y: " + mouseYCoord.toString, Color.red)
    font.drawString(2, getHeight - 225, "Mouse X: " + Mouse.getX.toString, Color.red)
    font.drawString(130, getHeight - 225, "Y: " + (getHeight - Mouse.getY).toString, Color.red)
    font.drawString(2, getHeight - 205, "Version: " + getVersion, Color.white)
    font.drawString(900, getHeight - 205, "Episode: " + getEpisode, Color.white)
    font.drawString(990, getHeight - 205, "Level: " + getLevel, Color.white)
    val name = getPlayer.name + " " + getPlayer.title
    font.drawString(2, 2, name, Color.white)
    if (Mouse.getX < 1057 && (getHeight - Mouse.getY) < 545) drawInfoBox
    if (getGameState == GameState.LEVEL) drawLevel
    if (getLastMonster != null) {
      drawQuadTex(blackBorder, 33*32/2-204, 2, 408, 4)
      drawQuadTex(blackBorder, 33*32/2-204, 31, 408, 4)
      drawQuadTex(blackBorder, 33*32/2-204, 2, 4, 33)
      drawQuadTex(blackBorder, 33*32/2+200, 2, 4, 33)
      val monsterStatus = getLastMonster.name + " " + getLastMonster.health.toInt + "/" + MonsterType.maxHP(getLastMonster.mType)
      var HPWidth = getLastMonster.health / MonsterType.maxHP(getLastMonster.mType)
      if (HPWidth > 1) HPWidth = 1
      if (HPWidth < 0) HPWidth = 0
      drawQuadTex(HPBar, 33*32/2-200, 6, (400*HPWidth).toInt, 25)
      font.drawString((33*32 - font.getWidth(monsterStatus))/2, 2, monsterStatus)
    }
  }
  
  /** Draw gamelog to bottom of the screen */
  def drawLog() {
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
      font.drawString(10, getHeight - 20 - num * 20, line, Color.black)
    }
  }
  
  /** Draw sidebar to right of the screen */
  def drawSideBar() {
    val middle = 33*32 + (getWidth - 33*32)/2
    var h = 0
    
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
    
    drawQuadTex(tempUIBackground, (getWidth-tempUIBackground.getImageWidth)/2, 
        (getHeight-tempUIBackground.getImageHeight)/2, tempUIBackground.getImageWidth, 
        tempUIBackground.getImageHeight)
    
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
      drawQuadTex(buttonXP.tex, buttonXP.getDrawX, buttonXP.getDrawY, buttonXP.tex.getImageWidth, 
          buttonXP.tex.getImageHeight)
    else drawQuadTex(buttonXP.tex2, buttonXP.getDrawX, buttonXP.getDrawY, 
        buttonXP.tex2.getImageWidth, buttonXP.tex2.getImageHeight)
        
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
    if (getPlayer.slotItem != null) drawQuadTex(getPlayer.slotItem.imageEquipped, 
        middle+getPlayer.slotItem.imageEquipped.getImageWidth*3/2, h, 
        getPlayer.slotItem.imageEquipped.getImageWidth, getPlayer.slotItem.imageEquipped.getImageHeight)
    
    h += 64
    val heroLevel = "Hero level: " + getPlayer.totalLevel
    font.drawString(middle-80, h, heroLevel, Color.black)
    
    h += 20
    val gold = "Gold: " + getPlayer.gold.toInt
    font.drawString(middle-80, h, gold, Color.black)
    
    h += 20
    val piety = "Piety: " + getPlayer.piety.toInt
    font.drawString(middle-80, h, piety, Color.black)
    
    h += 40
    drawQuadTex(buttonQuit.tex, buttonQuit.getDrawX, buttonQuit.getDrawY, buttonQuit.tex.getImageWidth, buttonQuit.tex.getImageHeight)
  }
  
  /** Draws Tiles */
  def drawTiles() {
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
    drawQuadTex(tempLevelBackground, (getWidth-tempLevelBackground.getImageWidth)/2, 
        (getHeight-tempLevelBackground.getImageHeight)/2, tempLevelBackground.getImageWidth, 
        tempLevelBackground.getImageHeight)
    val title = "Choose level:"
    var h = 20
    var v = 150
    
    fontMenu.drawString(v, 35, title, Color.black)
    if (getPlayer.experience >= xpNeededForLevel(getPlayer.charity)) 
      drawQuadTex(buttonCharity.tex, buttonCharity.getDrawX, buttonCharity.getDrawY, 
          buttonCharity.tex.getImageWidth, buttonCharity.tex.getImageHeight)
    else drawQuadTex(buttonCharity.tex2, buttonCharity.getDrawX, buttonCharity.getDrawY, 
        buttonCharity.tex2.getImageWidth, buttonCharity.tex2.getImageHeight)
    h += 70
    if (getPlayer.experience >= xpNeededForLevel(getPlayer.diligence)) 
      drawQuadTex(buttonDiligence.tex, buttonDiligence.getDrawX, buttonDiligence.getDrawY, 
          buttonDiligence.tex.getImageWidth, buttonDiligence.tex2.getImageHeight)
    else drawQuadTex(buttonDiligence.tex2, buttonDiligence.getDrawX, buttonDiligence.getDrawY, 
        buttonDiligence.tex2.getImageWidth, buttonDiligence.tex2.getImageHeight)
    h += 70
    if (getPlayer.experience >= xpNeededForLevel(getPlayer.humility)) 
      drawQuadTex(buttonHumility.tex, buttonHumility.getDrawX, buttonHumility.getDrawY, 
          buttonHumility.tex.getImageWidth, buttonHumility.tex.getImageHeight)
    else drawQuadTex(buttonHumility.tex2, buttonHumility.getDrawX, buttonHumility.getDrawY, 
        buttonHumility.tex2.getImageWidth, buttonHumility.tex2.getImageHeight)
    h += 70
    if (getPlayer.experience >= xpNeededForLevel(getPlayer.kindness)) 
      drawQuadTex(buttonKindness.tex, buttonKindness.getDrawX, buttonKindness.getDrawY, 
          buttonKindness.tex.getImageWidth, buttonKindness.tex.getImageHeight)
    else drawQuadTex(buttonKindness.tex2, buttonKindness.getDrawX, buttonKindness.getDrawY, 
        buttonKindness.tex2.getImageWidth, buttonKindness.tex2.getImageHeight)
    h += 70
    if (getPlayer.experience >= xpNeededForLevel(getPlayer.patience)) 
      drawQuadTex(buttonPatience.tex, buttonPatience.getDrawX, buttonPatience.getDrawY, 
          buttonPatience.tex.getImageWidth, buttonPatience.tex.getImageHeight)
    else drawQuadTex(buttonPatience.tex2, buttonPatience.getDrawX, buttonPatience.getDrawY, 
        buttonPatience.tex2.getImageWidth, buttonPatience.tex2.getImageHeight)
    h += 70
    if (getPlayer.experience >= xpNeededForLevel(getPlayer.temperance)) 
      drawQuadTex(buttonTemperance.tex, buttonTemperance.getDrawX, buttonTemperance.getDrawY, 
          buttonTemperance.tex.getImageWidth, buttonTemperance.tex.getImageHeight)
    else drawQuadTex(buttonTemperance.tex2, buttonTemperance.getDrawX, buttonTemperance.getDrawY, 
        buttonTemperance.tex2.getImageWidth, buttonTemperance.tex2.getImageHeight)
    h += 70
    if (getPlayer.experience >= xpNeededForLevel(getPlayer.zeal)) 
      drawQuadTex(buttonZeal.tex, buttonZeal.getDrawX, buttonZeal.getDrawY, 
          buttonZeal.tex.getImageWidth, buttonZeal.tex.getImageHeight)
    else drawQuadTex(buttonZeal.tex2, buttonZeal.getDrawX, buttonZeal.getDrawY, 
        buttonZeal.tex2.getImageWidth, buttonZeal.tex2.getImageHeight)
    h += 70
    drawQuadTex(buttonBackLVL.tex, buttonBackLVL.getDrawX, buttonBackLVL.getDrawY, 
        buttonBackLVL.tex.getImageWidth, buttonBackLVL.tex.getImageHeight)
    
    h = 128
    v = 400
    for (lev <- Range(0,10)) {
      v += 16
      if (lev + 1 <= getPlayer.charity) drawQuadTex(level, v, h, level.getImageWidth, level.getImageHeight)
      else drawQuadTex(level2, v, h, level2.getImageWidth, level2.getImageHeight)
    }
    v += 32
    font.drawString(v, h, if (getPlayer.charity < 10) "Costs: " + xpNeededForLevel(getPlayer.charity) else "", Color.black)
    v += 128
    font.drawString(v, h, "Charity increases piety gain.", Color.black)
    h += 70
    v = 400
    for (lev <- Range(0,10)) {
      v += 16
      if (lev + 1 <= getPlayer.diligence) drawQuadTex(level, v, h, level.getImageWidth, level.getImageHeight)
      else drawQuadTex(level2, v, h, level2.getImageWidth, level2.getImageHeight)
    }
    v += 32
    font.drawString(v, h, if (getPlayer.diligence < 10) "Costs: " + xpNeededForLevel(getPlayer.diligence) else "", Color.black)
    v += 128
    font.drawString(v, h, "Diligence increases gold and xp gain.", Color.black)
    h += 70
    v = 400
    for (lev <- Range(0,10)) {
      v += 16
      if (lev + 1 <= getPlayer.humility) drawQuadTex(level, v, h, level.getImageWidth, level.getImageHeight)
      else drawQuadTex(level2, v, h, level2.getImageWidth, level2.getImageHeight)
    }
    v += 32
    font.drawString(v, h, if (getPlayer.humility < 10) "Costs: " + xpNeededForLevel(getPlayer.humility) else "", Color.black)
    v += 128
    font.drawString(v, h, "Humility increases dodge chance.", Color.black)
    h += 70
    v = 400
    for (lev <- Range(0,10)) {
      v += 16
      if (lev + 1 <= getPlayer.kindness) drawQuadTex(level, v, h, level.getImageWidth, level.getImageHeight)
      else drawQuadTex(level2, v, h, level2.getImageWidth, level2.getImageHeight)
    }
    v += 32
    font.drawString(v, h, if (getPlayer.kindness < 10) "Costs: " + xpNeededForLevel(getPlayer.kindness) else "", Color.black)
    v += 128
    font.drawString(v, h, "Kindness increases maximum health.", Color.black)
    h += 70
    v = 400
    for (lev <- Range(0,10)) {
      v += 16
      if (lev + 1 <= getPlayer.patience) drawQuadTex(level, v, h, level.getImageWidth, level.getImageHeight)
      else drawQuadTex(level2, v, h, level2.getImageWidth, level2.getImageHeight)
    }
    v += 32
    font.drawString(v, h, if (getPlayer.patience < 10) "Costs: " + xpNeededForLevel(getPlayer.patience) else "", Color.black)
    v += 128
    font.drawString(v, h, "Patience increases weight limit.", Color.black)
    h += 70
    v = 400
    for (lev <- Range(0,10)) {
      v += 16
      if (lev + 1 <= getPlayer.temperance) drawQuadTex(level, v, h, level.getImageWidth, level.getImageHeight)
      else drawQuadTex(level2, v, h, level2.getImageWidth, level2.getImageHeight)
    }
    v += 32
    font.drawString(v, h, if (getPlayer.temperance < 10) "Costs: " + xpNeededForLevel(getPlayer.temperance) else "", Color.black)
    v += 128
    font.drawString(v, h, "Temperance increases accuracy.", Color.black)
    h += 70
    v = 400
    for (lev <- Range(0,10)) {
      v += 16
      if (lev + 1 <= getPlayer.zeal) drawQuadTex(level, v, h, level.getImageWidth, level.getImageHeight)
      else drawQuadTex(level2, v, h, level2.getImageWidth, level2.getImageHeight)
    }
    v += 32
    font.drawString(v, h, if (getPlayer.zeal < 10) "Costs: " + xpNeededForLevel(getPlayer.zeal) else "", Color.black)
    v += 128
    font.drawString(v, h, "Zeal increases damage.", Color.black)
  }
  
}