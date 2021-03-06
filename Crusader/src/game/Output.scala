package game

import java.awt.Font
import java.io.{IOException, File}

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
      Display.setTitle("Crusader version " + getVersion)
      Display.create()
      Display.setVSyncEnabled(vSync)
    } catch {
      case e:LWJGLException =>
        e.printStackTrace()
        Display.destroy()
        System.exit(1)
    }
    
    glMatrixMode(GL_PROJECTION)
    glOrtho(0, getWidth, getHeight, 0, 1, -1)
    glMatrixMode(GL_MODELVIEW)
    glEnable(GL_BLEND)
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA)

    
    drawQuadTex(background, (getWidth-background.getImageWidth)/2, 
        (getHeight-background.getImageHeight)/2, background.getImageWidth, background.getImageHeight)
    drawQuadTex(loading, (getWidth-loading.getImageWidth)/2, (getHeight-loading.getImageHeight)/2, 
        loading.getImageWidth, loading.getImageHeight)
    Display.update
    
    glEnable(GL_TEXTURE_2D)
    glDisable(GL_DEPTH_TEST)
    glShadeModel(GL_SMOOTH)
    glDisable(GL_LIGHTING)
    glClearColor(0.0f, 0.0f, 0.0f, 0.0f)
    glClearDepth(1)
    glViewport(0,0,getWidth,getHeight)
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST)
    org.lwjgl.input.Keyboard.enableRepeatEvents(true)
    
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
        (getHeight-emptyBackground.getImageHeight)/2, emptyBackground.getImageWidth, 
        emptyBackground.getImageHeight)
    if (font.getWidth(getPlayer.name) >= 700) {
      font.drawString(getWidth/2-font.getWidth("Name is too long")/2, getHeight*5/9, 
          "Name is too long", Color.red)
      drawQuadTex(buttonNewGameChar.tex2, buttonNewGameChar.getDrawX, buttonNewGameChar.getDrawY, 
          buttonNewGameChar.tex2.getImageWidth, buttonNewGameChar.tex2.getImageHeight)
    }
    else if (getPlayer.name == "") drawQuadTex(buttonNewGameChar.tex2, buttonNewGameChar.getDrawX, 
        buttonNewGameChar.getDrawY, buttonNewGameChar.tex2.getImageWidth, 
        buttonNewGameChar.tex2.getImageHeight)
    else if (buttonNewGameChar.isMouseWithin) drawQuadTex(buttonNewGameChar.tex3, 
        buttonNewGameChar.getDrawX, buttonNewGameChar.getDrawY, buttonNewGameChar.tex3.getImageWidth, 
        buttonNewGameChar.tex3.getImageHeight)
    else drawQuadTex(buttonNewGameChar.tex, buttonNewGameChar.getDrawX, buttonNewGameChar.getDrawY, 
        buttonNewGameChar.tex.getImageWidth, buttonNewGameChar.tex.getImageHeight)
    if (buttonBackChar.isMouseWithin) drawQuadTex(buttonBackChar.tex3, buttonBackChar.getDrawX, 
        buttonBackChar.getDrawY, buttonBackChar.tex3.getImageWidth, buttonBackChar.tex3.getImageHeight)
    else drawQuadTex(buttonBackChar.tex, buttonBackChar.getDrawX, buttonBackChar.getDrawY, 
        buttonBackChar.tex.getImageWidth, buttonBackChar.tex.getImageHeight)
    if (buttonRandom.isMouseWithin) drawQuadTex(buttonRandom.tex3, buttonRandom.getDrawX, 
        buttonRandom.getDrawY, buttonRandom.tex3.getImageWidth, buttonRandom.tex3.getImageHeight)
    else drawQuadTex(buttonRandom.tex, buttonRandom.getDrawX, buttonRandom.getDrawY, 
        buttonRandom.tex.getImageWidth, buttonRandom.tex.getImageHeight)
    fontMenu.drawString(getWidth/2-fontMenu.getWidth("Enter name:")/2, getHeight*3/9, 
        "Enter name:", Color.black)
    font.drawString(getWidth/2-font.getWidth(getPlayer.name)/2, getHeight*4/9, 
        getPlayer.name, Color.black)
    //font.drawString(2, getHeight - 24, "Mouse X: " + Mouse.getX.toString, Color.red)
    //font.drawString(130, getHeight - 24, "Y: " + (getHeight - Mouse.getY).toString, Color.red)
  }
  
  /** Draw credits */
  def drawCredits() {
    glClear(GL_COLOR_BUFFER_BIT)
    drawQuadTex(emptyBackground, (getWidth-emptyBackground.getImageWidth)/2, 
        (getHeight-emptyBackground.getImageHeight)/2, emptyBackground.getImageWidth, emptyBackground.getImageHeight)
    val text1 = "Credits:"
    font.drawString(getWidth/2-font.getWidth(text1)/2, getHeight/3, text1, Color.black)
    val text2 = "Project lead & Programming: Antti Karkinen"
    font.drawString(getWidth/2-font.getWidth(text2)/2, getHeight/3+25, text2, Color.black)
    val text3 = "Artist: Johanna Karkinen"
    font.drawString(getWidth/2-font.getWidth(text3)/2, getHeight/3+50, text3, Color.black)
    
    if (buttonBackCre.isMouseWithin) {
      drawQuadTex(buttonBackCre.tex3, buttonBackCre.getDrawX, buttonBackCre.getDrawY, 
          buttonBackCre.tex3.getImageWidth, buttonBackCre.tex3.getImageHeight)
    }
    else drawQuadTex(buttonBackCre.tex, buttonBackCre.getDrawX, buttonBackCre.getDrawY, 
        buttonBackCre.tex.getImageWidth, buttonBackCre.tex.getImageHeight)
  }
  
  /** Draw options */
  def drawOptions() {
    glClear(GL_COLOR_BUFFER_BIT)
    drawQuadTex(emptyBackground, (getWidth-emptyBackground.getImageWidth)/2, 
        (getHeight-emptyBackground.getImageHeight)/2, emptyBackground.getImageWidth, emptyBackground.getImageHeight)
    val text1 = "Resolution is " + getDisplayMode.getWidth + "x" + getDisplayMode.getHeight
    font.drawString(getWidth/2-font.getWidth(text1)/2, getHeight/6, text1, Color.black)
    val text2 = "ALT+ENTER to change to fullscreen."
    font.drawString(getWidth/2-font.getWidth(text2)/2, getHeight/6+25, text2, Color.black)
    val text3 = "Keys:"
    font.drawString(getWidth/2-font.getWidth(text3)/2, getHeight/6+75, text3, Color.black)
    val text4 = "Numpad/arrows = Movement"
    font.drawString(getWidth/2-font.getWidth(text4)/2, getHeight/6+100, text4, Color.black)
    val text5 = "Esc = Save&Quit"
    font.drawString(getWidth/2-font.getWidth(text5)/2, getHeight/6+125, text5, Color.black)
    val text6 = "Q = Level up"
    font.drawString(getWidth/2-font.getWidth(text6)/2, getHeight/6+150, text6, Color.black)
    val text7 = "W = Wait/Buy item"
    font.drawString(getWidth/2-font.getWidth(text7)/2, getHeight/6+175, text7, Color.black)
    val text8 = "E = Use item"
    font.drawString(getWidth/2-font.getWidth(text8)/2, getHeight/6+200, text8, Color.black)
    val text9 = "R = Pray"
    font.drawString(getWidth/2-font.getWidth(text9)/2, getHeight/6+225, text9, Color.black)
    
    if (buttonBackOpt.isMouseWithin) {
      drawQuadTex(buttonBackOpt.tex3, buttonBackOpt.getDrawX, buttonBackOpt.getDrawY, 
          buttonBackOpt.tex3.getImageWidth, buttonBackOpt.tex3.getImageHeight)
    }
    else drawQuadTex(buttonBackOpt.tex, buttonBackOpt.getDrawX, buttonBackOpt.getDrawY, 
        buttonBackOpt.tex.getImageWidth, buttonBackOpt.tex.getImageHeight)
  }
  
  /** Draw the main menu */
  def drawMainMenu() {
    glClear(GL_COLOR_BUFFER_BIT)
    drawQuadTex(background, (getWidth-background.getImageWidth)/2, 
        (getHeight-background.getImageHeight)/2, background.getImageWidth, background.getImageHeight)
    if (new File("save.dat").exists && buttonContinue.isMouseWithin) drawQuadTex(buttonContinue.tex3, 
        buttonContinue.getDrawX.toInt, buttonContinue.getDrawY.toInt, buttonContinue.tex3.getImageWidth, 
        buttonContinue.tex3.getImageHeight)
    else if (new File("save.dat").exists) drawQuadTex(buttonContinue.tex, buttonContinue.getDrawX.toInt, 
        buttonContinue.getDrawY.toInt, buttonContinue.tex.getImageWidth, buttonContinue.tex.getImageHeight)
    else drawQuadTex(buttonContinue.tex2, buttonContinue.getDrawX, buttonContinue.getDrawY, 
        buttonContinue.tex2.getImageWidth, buttonContinue.tex2.getImageHeight)
    if (buttonNewGameMenu.isMouseWithin) {
      drawQuadTex(buttonNewGameMenu.tex3, buttonNewGameMenu.getDrawX, buttonNewGameMenu.getDrawY, 
          buttonNewGameMenu.tex3.getImageWidth, buttonNewGameMenu.tex3.getImageHeight)
    }
    else drawQuadTex(buttonNewGameMenu.tex, buttonNewGameMenu.getDrawX, buttonNewGameMenu.getDrawY, 
        buttonNewGameMenu.tex.getImageWidth, buttonNewGameMenu.tex.getImageHeight)
    if (buttonOptions.isMouseWithin) drawQuadTex(buttonOptions.tex3, buttonOptions.getDrawX, 
        buttonOptions.getDrawY, buttonOptions.tex3.getImageWidth, buttonOptions.tex3.getImageHeight)
    else drawQuadTex(buttonOptions.tex, buttonOptions.getDrawX, buttonOptions.getDrawY, 
        buttonOptions.tex.getImageWidth, buttonOptions.tex.getImageHeight)
    if (buttonCredits.isMouseWithin) drawQuadTex(buttonCredits.tex3, buttonCredits.getDrawX, 
        buttonCredits.getDrawY, buttonCredits.tex3.getImageWidth, buttonCredits.tex3.getImageHeight)
    else drawQuadTex(buttonCredits.tex, buttonCredits.getDrawX, buttonCredits.getDrawY, 
        buttonCredits.tex.getImageWidth, buttonCredits.tex.getImageHeight)
    if (buttonExit.isMouseWithin) drawQuadTex(buttonExit.tex3, buttonExit.getDrawX, buttonExit.getDrawY, 
        buttonExit.tex3.getImageWidth, buttonExit.tex3.getImageHeight)
    else drawQuadTex(buttonExit.tex, buttonExit.getDrawX, buttonExit.getDrawY, 
        buttonExit.tex.getImageWidth, buttonExit.tex.getImageHeight)
    fontMenu.drawString(920, getHeight/10, "Crusader", Color.black)        
    //font.drawString(2, getHeight - 24, "Mouse X: " + Mouse.getX.toString, Color.red)
    //font.drawString(130, getHeight - 24, "Y: " + (getHeight - Mouse.getY).toString, Color.red)
    font.drawString(getWidth - 10 - font.getWidth("Version: " + getVersion), getHeight - 24, "Version: " + getVersion, Color.black)
  }
  
  /** Draw game screen */
  def drawGame() {
    mouseXCoord = (Mouse.getX * 1.0 / 32 + getPlayer.getX - 16).toInt
    mouseYCoord = ((getHeight - Mouse.getY) * 1.0 / 32 + getPlayer.getY - 8).toInt
    glClear(GL_COLOR_BUFFER_BIT)
    drawQuadTex(UIBackground2, (getWidth-UIBackground2.getImageWidth)/2, 
        (getHeight-UIBackground2.getImageHeight)/2, UIBackground2.getImageWidth, UIBackground2.getImageHeight)
    getGrid.FOV
    drawTiles
    drawSideBar
    drawLog
    drawFog
    font.drawString(2, getHeight - 225, "X: " + getPlayer.getX.toString + " Y: " + getPlayer.getY.toString, Color.white)
    //font.drawString(2, getHeight - 265, "Mouse X: " + mouseXCoord.toString, Color.red)
    //font.drawString(130, getHeight - 265, "Y: " + mouseYCoord.toString, Color.red)
    //font.drawString(2, getHeight - 245, "Mouse X: " + Mouse.getX.toString, Color.red)
    //font.drawString(130, getHeight - 245, "Y: " + (getHeight - Mouse.getY).toString, Color.red)
    font.drawString(2, getHeight - 205, "Turn: " + getTurn, Color.white)
    val level = "Episode: " + getEpisode + " Level: " + getLevel
    font.drawString(1046-font.getWidth(level), getHeight - 205, level, Color.white)
    val name = getPlayer.name + " " + getPlayer.title
    font.drawString(2, 2, name, Color.white)
    val heroLevel = "Hero level: " + getPlayer.totalLevel.toInt
    font.drawString(1046-font.getWidth(heroLevel), 2, heroLevel, Color.white)
    val gold = "Gold: " + getPlayer.gold.toInt
    font.drawString(1046-font.getWidth(gold), 22, gold, Color.white)
    val piety = "Piety: " + getPlayer.piety.toInt
    font.drawString(1046-font.getWidth(piety), 42, piety, Color.white)
    drawObjectsUnderMouse
    if (getLastMonster != null) {
      drawQuadTex(blackBorder, 33*32/2-204, 34, 408, 4)
      drawQuadTex(blackBorder, 33*32/2-204, 63, 408, 4)
      drawQuadTex(blackBorder, 33*32/2-204, 34, 4, 33)
      drawQuadTex(blackBorder, 33*32/2+200, 34, 4, 33)
      val monsterStatus = getLastMonster.name.toUpperCase.head + getLastMonster.name.tail + 
      " " + getLastMonster.health.toInt + "/" + MonsterType.maxHP(getLastMonster.mType).toInt
      var HPWidth = getLastMonster.health / MonsterType.maxHP(getLastMonster.mType)
      if (HPWidth > 1) HPWidth = 1
      if (HPWidth < 0) HPWidth = 0
      drawQuadTex(HPBar, 33*32/2-200, 38, (400*HPWidth).toInt, 25)
      font.drawString((33*32 - font.getWidth(monsterStatus))/2, 34, monsterStatus)
    }
    if (getGameState == GameState.LEVEL) drawLevel
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
    
    var tempLog = Buffer[Buffer[(String, Color)]]()
    if (getGameLog.size <= 7) tempLog = getGameLog
    else for (line <- Range(getGameLog.size - 7 + mouseScrollBonus, getGameLog.size + mouseScrollBonus)) tempLog.append(getGameLog.apply(line))
    
    var x = 0
    var y = 0
    for (line <- tempLog.reverse) {
      x = 0
      y += 20
      for (text <- line) {
        font.drawString(10 + x, getHeight - 20 - y, text._1, newColor(text._2))
        x += font.getWidth(text._1)
      }
    }
  }
  
  /** Draw sidebar to right of the screen */
  def drawSideBar() {
    val middle = 33*32 + (getWidth - 33*32)/2
    var h = 0
    
    var HPHeight = getPlayer.health / getPlayer.maxHealth
    if (HPHeight > 1) HPHeight = 1
    if (HPHeight < 0) HPHeight = 0
    drawQuadTex(HPBar, middle-256/2+52, 46 + (116 * (1-HPHeight)).toInt, 152, (116 * HPHeight).toInt)
    
    var XPWidth = getPlayer.experience*1.0 / getPlayer.smallestLevel
    if (XPWidth > 1) XPWidth = 1
    if (XPWidth < 0) XPWidth = 0
    if (getPlayer.experience >= getPlayer.smallestLevel && buttonXP.isMouseWithin) drawQuadTex(XPBar3, middle-256/2+40, 176, (176*XPWidth).toInt, 122)
    else if (getPlayer.experience >= getPlayer.smallestLevel) drawQuadTex(XPBar2, middle-256/2+40, 176, (176*XPWidth).toInt, 122)
    else drawQuadTex(XPBar, middle-256/2+40, 176, (176*XPWidth).toInt, 122)
    
    drawQuadTex(UIBackground, (getWidth-UIBackground.getImageWidth)/2, 
        (getHeight-UIBackground.getImageHeight)/2, UIBackground.getImageWidth, 
        UIBackground.getImageHeight)
        
    h += 80
    val hptext = "HP: " + getPlayer.health.toInt + "/" + getPlayer.maxHealth.toInt
    font.drawString(middle-font.getWidth(hptext)/2, h, hptext, Color.black)
    
    h += 96
    val xptext = "XP: " + getPlayer.experience.toInt + "/" + getPlayer.smallestLevel.toInt
    font.drawString(middle-font.getWidth(xptext)/2, h, xptext, Color.black)
        
    if (buttonPray.isMouseWithin) drawQuadTex(buttonPray.tex3, buttonPray.getDrawX, 
        buttonPray.getDrawY, buttonPray.tex3.getImageWidth, buttonPray.tex3.getImageHeight)
    else drawQuadTex(buttonPray.tex2, buttonPray.getDrawX, buttonPray.getDrawY, 
        buttonPray.tex2.getImageWidth, buttonPray.tex2.getImageHeight)
    
    h += 130
    if (getPlayer.slotWeapon != null) drawQuadTex(getPlayer.slotWeapon.image, 
        middle-getPlayer.slotWeapon.image.getImageWidth*5/2, h, 
        getPlayer.slotWeapon.image.getImageWidth, getPlayer.slotWeapon.image.getImageHeight)
    if (getPlayer.slotArmor != null) drawQuadTex(getPlayer.slotArmor.image, 
        middle-getPlayer.slotArmor.image.getImageWidth/2, h, 
        getPlayer.slotArmor.image.getImageWidth, getPlayer.slotArmor.image.getImageHeight)
    if (getPlayer.slotShield != null) drawQuadTex(getPlayer.slotShield.image, 
        middle+getPlayer.slotShield.image.getImageWidth*3/2, h, 
        getPlayer.slotShield.image.getImageWidth, getPlayer.slotShield.image.getImageHeight)
    
    h += 54
    if (getPlayer.slotRing != null) drawQuadTex(getPlayer.slotRing.image, 
        middle-getPlayer.slotRing.image.getImageWidth*5/2, h, 
        getPlayer.slotRing.image.getImageWidth, getPlayer.slotRing.image.getImageHeight)
    if (getPlayer.slotAmulet != null) drawQuadTex(getPlayer.slotAmulet.image, 
        middle-getPlayer.slotAmulet.image.getImageWidth/2, h, 
        getPlayer.slotAmulet.image.getImageWidth, getPlayer.slotAmulet.image.getImageHeight)
    if (getPlayer.slotUseable != null) drawQuadTex(getPlayer.slotUseable.imageEquipped, 
        middle+getPlayer.slotUseable.imageEquipped.getImageWidth*3/2, h, 
        getPlayer.slotUseable.imageEquipped.getImageWidth, getPlayer.slotUseable.imageEquipped.getImageHeight)
    
    h += 155
    drawMinimap(middle, h)
    
    if (buttonQuit.isMouseWithin) drawQuadTex(buttonQuit.tex3, buttonQuit.getDrawX, 
        buttonQuit.getDrawY, buttonQuit.tex3.getImageWidth, buttonQuit.tex3.getImageHeight)
    else drawQuadTex(buttonQuit.tex, buttonQuit.getDrawX, buttonQuit.getDrawY, 
        buttonQuit.tex.getImageWidth, buttonQuit.tex.getImageHeight)
  }
  
  /** Draw minimap */
  def drawMinimap(middleX: Int, middleY: Int) {
    val mapSize = getGrid.getSize
    for (i <- Range(0,mapSize)) {
      for (j <- Range(0,mapSize)) {
        getGrid.getTile(i, j) match {
          case t if (t.explored && t.getType == TileType.FLOOR && 
              getPlayer.getX == t.getX && getPlayer.getY == t.getY) => 
            drawQuadTex(minimapPlayer, middleX-mapSize*2+i*4, middleY-mapSize*2+j*4, 4, 4)
          case t if (t.explored && t.getType == TileType.FLOOR && 
              getGrid.getAltar.getX == t.getX && getGrid.getAltar.getY == t.getY) => 
                drawQuadTex(minimapAltar, middleX-mapSize*2+i*4, middleY-mapSize*2+j*4, 4, 4)
          case t if (t.explored && t.visible && t.getType == TileType.FLOOR) => {
              var whatToDraw = ""
              for (obj <- t.getObjectList) {
                if (obj.isInstanceOf[Item]) whatToDraw = "item"
                else if (obj.isInstanceOf[Monster]) whatToDraw = "monster"
                else if (obj match {
                  case pasobj: PassiveObject if (pasobj.pType == PassiveType.STAIRS) => true
                  case _ => {false}
                  }) whatToDraw = "stairs"
                else if (obj.blockMovement) whatToDraw = "wall"
              }
              if (whatToDraw == "monster" && t.visible) 
                drawQuadTex(minimapEnemy, middleX-mapSize*2+i*4, middleY-mapSize*2+j*4, 4, 4)
              else if (whatToDraw == "item") 
                drawQuadTex(minimapItem, middleX-mapSize*2+i*4, middleY-mapSize*2+j*4, 4, 4)
              else if (whatToDraw == "wall") 
                drawQuadTex(minimapWall, middleX-mapSize*2+i*4, middleY-mapSize*2+j*4, 4, 4)
              else if (whatToDraw == "stairs")
                drawQuadTex(minimapStairs, middleX-mapSize*2+i*4, middleY-mapSize*2+j*4, 4, 4)
              else drawQuadTex(minimapFloor, middleX-mapSize*2+i*4, middleY-mapSize*2+j*4, 4, 4)
          }
          case t if (t.explored && t.getType == TileType.WALL) => 
            drawQuadTex(minimapWall, middleX-mapSize*2+i*4, middleY-mapSize*2+j*4, 4, 4)
          case t if (t.explored && (t.getType == TileType.DJINNDOORH || t.getType == TileType.DJINNDOORV || 
              t.getType == TileType.DJINNFLOOR || t.getType == TileType.DJINNWALL)) => 
            drawQuadTex(minimapDjinn, middleX-mapSize*2+i*4, middleY-mapSize*2+j*4, 4, 4)
          case t if (t.explored) => {
            var s = false
            for (obj <- t.getObjectList) obj match {
              case pasobj: PassiveObject if (pasobj.pType == PassiveType.STAIRS) => s = true
              case _ => {}
            }
            if (s) drawQuadTex(minimapStairs, middleX-mapSize*2+i*4, middleY-mapSize*2+j*4, 4, 4)
            else drawQuadTex(minimapFog, middleX-mapSize*2+i*4, middleY-mapSize*2+j*4, 4, 4)
          }
          case _ => {}
        }
      }
    }
    
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
    for (list <- List(getEquipmentList, getUseableList, getMonsterList, getPassiveObjectList)) {
      for (obj <- list) {
        if (obj.xDif(getPlayer) <= 16 && obj.yDif(getPlayer) <= 8 && 
            getGrid.getTile(obj.getX, obj.getY).explored) obj.draw
      }
    }
  }
  
  /** Draw fog */
  def drawFog() {
    for (i <- Range(0, getGrid.getSize())) {
      for (j <- Range(0, getGrid.getSize())) {
        if (getPlayer.xDif(i) <= 16 && getPlayer.yDif(j) <= 8) getGrid.getTile(i, j).drawFog
      }
    }
  }
  
  /** Draw info box about objects under mouse */
  def drawObjectsUnderMouse() {
    val tile = getGrid.getTile(mouseXCoord, mouseYCoord)
    var right = true
    var infoBox = Buffer[Buffer[(String, Color)]]()
    if (Mouse.getX < 1056 && (getHeight - Mouse.getY) < 544 &&  Mouse.isInsideWindow) 
      drawQuadTex(mouseSelector, (Mouse.getX * 1.0 / 32).toInt * 32, 
          ((getHeight - Mouse.getY) * 1.0 / 32).toInt * 32, mouseSelector.getImageWidth, 
          mouseSelector.getImageHeight)
    if (Mouse.getX < 1056 && (getHeight - Mouse.getY) < 544 && tile != null && tile.visible && 
        !tile.getObjectList.isEmpty) {
      infoBox = tile.getObjectList.head.infoBox
    }
    else if (Mouse.getX > 1088 && Mouse.getX < 1120 && (getHeight - Mouse.getY) > 308 && 
        (getHeight - Mouse.getY) < 338 && getPlayer.slotWeapon != null) 
      infoBox = getPlayer.slotWeapon.infoBox
    else if (Mouse.getX > 1088 && Mouse.getX < 1120 && (getHeight - Mouse.getY) > 308 && 
        (getHeight - Mouse.getY) < 338) infoBox = Buffer(Buffer(("Weapon slot", Color.black)))
    else if (Mouse.getX > 1152 && Mouse.getX < 1182 && (getHeight - Mouse.getY) > 308 && 
        (getHeight - Mouse.getY) < 338 && getPlayer.slotArmor != null) 
      infoBox = getPlayer.slotArmor.infoBox
    else if (Mouse.getX > 1152 && Mouse.getX < 1182 && (getHeight - Mouse.getY) > 308 && 
        (getHeight - Mouse.getY) < 338) infoBox = Buffer(Buffer(("Armor slot", Color.black)))
    else if (Mouse.getX > 1216 && Mouse.getX < 1248 && (getHeight - Mouse.getY) > 308 && 
        (getHeight - Mouse.getY) < 338 && getPlayer.slotShield != null) 
      infoBox = getPlayer.slotShield.infoBox
    else if (Mouse.getX > 1216 && Mouse.getX < 1248 && (getHeight - Mouse.getY) > 308 && 
        (getHeight - Mouse.getY) < 338) infoBox = Buffer(Buffer(("Shield slot", Color.black)))
    else if (Mouse.getX > 1088 && Mouse.getX < 1120 && (getHeight - Mouse.getY) > 361 && 
        (getHeight - Mouse.getY) < 391 && getPlayer.slotRing != null) 
      infoBox = getPlayer.slotRing.infoBox
    else if (Mouse.getX > 1088 && Mouse.getX < 1120 && (getHeight - Mouse.getY) > 361 && 
        (getHeight - Mouse.getY) < 391) infoBox = Buffer(Buffer(("Ring slot", Color.black)))
    else if (Mouse.getX > 1152 && Mouse.getX < 1182 && (getHeight - Mouse.getY) > 361 && 
        (getHeight - Mouse.getY) < 391 && getPlayer.slotAmulet != null) 
      infoBox = getPlayer.slotAmulet.infoBox
    else if (Mouse.getX > 1152 && Mouse.getX < 1182 && (getHeight - Mouse.getY) > 361 && 
        (getHeight - Mouse.getY) < 391) infoBox = Buffer(Buffer(("Amulet slot", Color.black)))
    else if (Mouse.getX > 1216 && Mouse.getX < 1248 && (getHeight - Mouse.getY) > 361 && 
        (getHeight - Mouse.getY) < 391 && getPlayer.slotUseable != null) 
      infoBox = getPlayer.slotUseable.infoBox
    else if (Mouse.getX > 1216 && Mouse.getX < 1248 && (getHeight - Mouse.getY) > 361 && 
        (getHeight - Mouse.getY) < 391) infoBox = Buffer(Buffer(("Item slot", Color.black)))
    
    var boxHeight = infoBox.size * 20
    var boxWidth = 0
    for (row <- infoBox) {
      var current = 0
      for (text <- row) current += font.getWidth(text._1)
      if (current >= boxWidth) boxWidth = current
      }
    
    if (Mouse.getX + boxWidth + 20 > getWidth) right = false
    
    if (right && !infoBox.isEmpty) {
      drawQuadTex(greyBackground, Mouse.getX + 18, getHeight - Mouse.getY, boxWidth + 4, boxHeight)
      var y = -5
      var x = 20
      for (row <- infoBox) {
        x = 20
        for (text <- row) {
          font.drawString(Mouse.getX + x, getHeight - Mouse.getY + y, text._1, text._2)
          x += font.getWidth(text._1)
        }
        y += 20
      }
    }
    else if (!infoBox.isEmpty) {
      drawQuadTex(greyBackground, Mouse.getX - 12 - boxWidth, getHeight - Mouse.getY - 5, 
          boxWidth + 4, boxHeight)
      var y = -10
      var x = -10 - boxWidth
      for (row <- infoBox) {
        x = -10 - boxWidth
        for (text <- row) {
          font.drawString(Mouse.getX + x, getHeight - Mouse.getY + y, text._1, text._2)
          x += font.getWidth(text._1)
        }
        y += 20
      }
    }
  }
  
  /** Draw the character level up screen */
  def drawLevel {
    drawQuadTex(LevelBackground, (getWidth-LevelBackground.getImageWidth)/2, 
        (getHeight-LevelBackground.getImageHeight)/2, LevelBackground.getImageWidth, 
        LevelBackground.getImageHeight)
    var h = 20
    var v = 150
    
    fontMenu.drawString(v, 35, "Choose level:", Color.black)
    font.drawString(v+440, h+35, "Experience: " + getPlayer.experience.toInt, Color.black)
    if (getPlayer.experience >= xpNeededForLevel(getPlayer.getCharity.toInt) && buttonCharity.isMouseWithin) 
      drawQuadTex(buttonCharity.tex3, buttonCharity.getDrawX, buttonCharity.getDrawY, 
          buttonCharity.tex3.getImageWidth, buttonCharity.tex3.getImageHeight)
    else if (getPlayer.experience >= xpNeededForLevel(getPlayer.getCharity.toInt)) 
      drawQuadTex(buttonCharity.tex, buttonCharity.getDrawX, buttonCharity.getDrawY, 
          buttonCharity.tex.getImageWidth, buttonCharity.tex.getImageHeight)
    else drawQuadTex(buttonCharity.tex2, buttonCharity.getDrawX, buttonCharity.getDrawY, 
        buttonCharity.tex2.getImageWidth, buttonCharity.tex2.getImageHeight)
    h += 70
    if (getPlayer.experience >= xpNeededForLevel(getPlayer.getDiligence.toInt) && buttonDiligence.isMouseWithin) 
      drawQuadTex(buttonDiligence.tex3, buttonDiligence.getDrawX, buttonDiligence.getDrawY, 
          buttonDiligence.tex3.getImageWidth, buttonDiligence.tex3.getImageHeight)
    else if (getPlayer.experience >= xpNeededForLevel(getPlayer.getDiligence.toInt)) 
      drawQuadTex(buttonDiligence.tex, buttonDiligence.getDrawX, buttonDiligence.getDrawY, 
          buttonDiligence.tex.getImageWidth, buttonDiligence.tex.getImageHeight)
    else drawQuadTex(buttonDiligence.tex2, buttonDiligence.getDrawX, buttonDiligence.getDrawY, 
        buttonDiligence.tex2.getImageWidth, buttonDiligence.tex2.getImageHeight)
    h += 70
    if (getPlayer.experience >= xpNeededForLevel(getPlayer.getHumility.toInt) && buttonHumility.isMouseWithin) 
      drawQuadTex(buttonHumility.tex3, buttonHumility.getDrawX, buttonHumility.getDrawY, 
          buttonHumility.tex3.getImageWidth, buttonHumility.tex3.getImageHeight)
    else if (getPlayer.experience >= xpNeededForLevel(getPlayer.getHumility.toInt)) 
      drawQuadTex(buttonHumility.tex, buttonHumility.getDrawX, buttonHumility.getDrawY, 
          buttonHumility.tex.getImageWidth, buttonHumility.tex.getImageHeight)
    else drawQuadTex(buttonHumility.tex2, buttonHumility.getDrawX, buttonHumility.getDrawY, 
        buttonHumility.tex2.getImageWidth, buttonHumility.tex2.getImageHeight)
    h += 70
    if (getPlayer.experience >= xpNeededForLevel(getPlayer.getKindness.toInt) && buttonKindness.isMouseWithin) 
      drawQuadTex(buttonKindness.tex3, buttonKindness.getDrawX, buttonKindness.getDrawY, 
          buttonKindness.tex3.getImageWidth, buttonKindness.tex3.getImageHeight)
    else if (getPlayer.experience >= xpNeededForLevel(getPlayer.getKindness.toInt)) 
      drawQuadTex(buttonKindness.tex, buttonKindness.getDrawX, buttonKindness.getDrawY, 
          buttonKindness.tex.getImageWidth, buttonKindness.tex.getImageHeight)
    else drawQuadTex(buttonKindness.tex2, buttonKindness.getDrawX, buttonKindness.getDrawY, 
        buttonKindness.tex2.getImageWidth, buttonKindness.tex2.getImageHeight)
    h += 70
    if (getPlayer.experience >= xpNeededForLevel(getPlayer.getPatience.toInt) && buttonPatience.isMouseWithin) 
      drawQuadTex(buttonPatience.tex3, buttonPatience.getDrawX, buttonPatience.getDrawY, 
          buttonPatience.tex3.getImageWidth, buttonPatience.tex3.getImageHeight)
    else if (getPlayer.experience >= xpNeededForLevel(getPlayer.getPatience.toInt)) 
      drawQuadTex(buttonPatience.tex, buttonPatience.getDrawX, buttonPatience.getDrawY, 
          buttonPatience.tex.getImageWidth, buttonPatience.tex.getImageHeight)
    else drawQuadTex(buttonPatience.tex2, buttonPatience.getDrawX, buttonPatience.getDrawY, 
        buttonPatience.tex2.getImageWidth, buttonPatience.tex2.getImageHeight)
    h += 70
    if (getPlayer.experience >= xpNeededForLevel(getPlayer.getTemperance.toInt) && buttonTemperance.isMouseWithin) 
      drawQuadTex(buttonTemperance.tex3, buttonTemperance.getDrawX, buttonTemperance.getDrawY, 
          buttonTemperance.tex3.getImageWidth, buttonTemperance.tex3.getImageHeight)
    else if (getPlayer.experience >= xpNeededForLevel(getPlayer.getTemperance.toInt)) 
      drawQuadTex(buttonTemperance.tex, buttonTemperance.getDrawX, buttonTemperance.getDrawY, 
          buttonTemperance.tex.getImageWidth, buttonTemperance.tex.getImageHeight)
    else drawQuadTex(buttonTemperance.tex2, buttonTemperance.getDrawX, buttonTemperance.getDrawY, 
        buttonTemperance.tex2.getImageWidth, buttonTemperance.tex2.getImageHeight)
    h += 70
    if (getPlayer.experience >= xpNeededForLevel(getPlayer.getZeal.toInt) && buttonZeal.isMouseWithin) 
      drawQuadTex(buttonZeal.tex3, buttonZeal.getDrawX, buttonZeal.getDrawY, 
          buttonZeal.tex3.getImageWidth, buttonZeal.tex3.getImageHeight)
    else if (getPlayer.experience >= xpNeededForLevel(getPlayer.getZeal.toInt)) 
      drawQuadTex(buttonZeal.tex, buttonZeal.getDrawX, buttonZeal.getDrawY, 
          buttonZeal.tex.getImageWidth, buttonZeal.tex.getImageHeight)
    else drawQuadTex(buttonZeal.tex2, buttonZeal.getDrawX, buttonZeal.getDrawY, 
        buttonZeal.tex2.getImageWidth, buttonZeal.tex2.getImageHeight)
    h += 70
    if (buttonBackLVL.isMouseWithin) drawQuadTex(buttonBackLVL.tex3, buttonBackLVL.getDrawX, 
        buttonBackLVL.getDrawY, buttonBackLVL.tex3.getImageWidth, buttonBackLVL.tex3.getImageHeight)
    else drawQuadTex(buttonBackLVL.tex, buttonBackLVL.getDrawX, buttonBackLVL.getDrawY, 
        buttonBackLVL.tex.getImageWidth, buttonBackLVL.tex.getImageHeight)
    
    h = 128
    v = 400
    for (lev <- Range(0,10)) {
      v += 16
      if (lev + 1 <= getPlayer.getCharity) drawQuadTex(level, v, h, level.getImageWidth, level.getImageHeight)
      else drawQuadTex(level2, v, h, level2.getImageWidth, level2.getImageHeight)
    }
    v += 32
    font.drawString(v, h, if (getPlayer.getCharity < 10) "Costs: " + xpNeededForLevel(getPlayer.getCharity.toInt) else "", Color.black)
    v += 128
    font.drawString(v, h, "Charity increases piety gain.", Color.black)
    h += 70
    v = 400
    for (lev <- Range(0,10)) {
      v += 16
      if (lev + 1 <= getPlayer.getDiligence) drawQuadTex(level, v, h, level.getImageWidth, level.getImageHeight)
      else drawQuadTex(level2, v, h, level2.getImageWidth, level2.getImageHeight)
    }
    v += 32
    font.drawString(v, h, if (getPlayer.getDiligence < 10) "Costs: " + xpNeededForLevel(getPlayer.getDiligence.toInt) else "", Color.black)
    v += 128
    font.drawString(v, h, "Diligence increases gold and xp gain.", Color.black)
    h += 70
    v = 400
    for (lev <- Range(0,10)) {
      v += 16
      if (lev + 1 <= getPlayer.getHumility) drawQuadTex(level, v, h, level.getImageWidth, level.getImageHeight)
      else drawQuadTex(level2, v, h, level2.getImageWidth, level2.getImageHeight)
    }
    v += 32
    font.drawString(v, h, if (getPlayer.getHumility < 10) "Costs: " + xpNeededForLevel(getPlayer.getHumility.toInt) else "", Color.black)
    v += 128
    font.drawString(v, h, "Humility increases dodge chance.", Color.black)
    h += 70
    v = 400
    for (lev <- Range(0,10)) {
      v += 16
      if (lev + 1 <= getPlayer.getKindness) drawQuadTex(level, v, h, level.getImageWidth, level.getImageHeight)
      else drawQuadTex(level2, v, h, level2.getImageWidth, level2.getImageHeight)
    }
    v += 32
    font.drawString(v, h, if (getPlayer.getKindness < 10) "Costs: " + xpNeededForLevel(getPlayer.getKindness.toInt) else "", Color.black)
    v += 128
    font.drawString(v, h, "Kindness increases maximum health.", Color.black)
    h += 70
    v = 400
    for (lev <- Range(0,10)) {
      v += 16
      if (lev + 1 <= getPlayer.getPatience) drawQuadTex(level, v, h, level.getImageWidth, level.getImageHeight)
      else drawQuadTex(level2, v, h, level2.getImageWidth, level2.getImageHeight)
    }
    v += 32
    font.drawString(v, h, if (getPlayer.getPatience < 10) "Costs: " + xpNeededForLevel(getPlayer.getPatience.toInt) else "", Color.black)
    v += 128
    font.drawString(v, h, "Patience increases weight limit.", Color.black)
    h += 70
    v = 400
    for (lev <- Range(0,10)) {
      v += 16
      if (lev + 1 <= getPlayer.getTemperance) drawQuadTex(level, v, h, level.getImageWidth, level.getImageHeight)
      else drawQuadTex(level2, v, h, level2.getImageWidth, level2.getImageHeight)
    }
    v += 32
    font.drawString(v, h, if (getPlayer.getTemperance < 10) "Costs: " + xpNeededForLevel(getPlayer.getTemperance.toInt) else "", Color.black)
    v += 128
    font.drawString(v, h, "Temperance increases accuracy.", Color.black)
    h += 70
    v = 400
    for (lev <- Range(0,10)) {
      v += 16
      if (lev + 1 <= getPlayer.getZeal) drawQuadTex(level, v, h, level.getImageWidth, level.getImageHeight)
      else drawQuadTex(level2, v, h, level2.getImageWidth, level2.getImageHeight)
    }
    v += 32
    font.drawString(v, h, if (getPlayer.getZeal < 10) "Costs: " + xpNeededForLevel(getPlayer.getZeal.toInt) else "", Color.black)
    v += 128
    font.drawString(v, h, "Zeal increases damage.", Color.black)
  }
  
}