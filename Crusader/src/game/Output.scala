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

/** Output is responsible for everything that is drawn on the screen.
 */
object Output {
  
  private val antiAlias: Boolean = true
  private val vSync: Boolean = false
  private val height: Int = Main.height
  private val width: Int = Main.width
  private val version: String = Main.version
  
  private var font: TrueTypeFont = null
  private var fontMenu: TrueTypeFont = null  
  private var awtFont: Font = null
  private var background:Texture = null
  private var continue:Texture = null
  private var continue2:Texture = null
  private var newgame:Texture = null
  private var quit:Texture = null
  private var tempHeart: Texture = null
  private var tempXp: Texture = null
  private var tempUIBackground: Texture = null
  private var tempBlack: Texture = null
  
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
    
    try {
      awtFont = Font.createFont(Font.TRUETYPE_FONT, 
          ResourceLoader.getResourceAsStream("data/Kingthings Petrock.ttf"))
      awtFont = awtFont.deriveFont(70f)
      fontMenu = new TrueTypeFont(awtFont, antiAlias)
      awtFont = Font.createFont(Font.TRUETYPE_FONT, 
          ResourceLoader.getResourceAsStream("data/Kingthings Petrock light.ttf"))
      awtFont = awtFont.deriveFont(24f)
      font = new TrueTypeFont(awtFont, antiAlias)
    } catch {
      case e: Throwable => e.printStackTrace()
    }
    
    background = loadTexture("Background")
    continue = loadTexture("Continue")
    continue2 = loadTexture("Continue2")
    newgame = loadTexture("Newgame")
    quit = loadTexture("Quit")
    tempHeart = loadTexture("tempHeart")
    tempXp = loadTexture("tempXp")
    tempUIBackground = loadTexture("tempUIBackground")
    tempBlack = loadTexture("tempBlack")
    
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
      tex = TextureLoader.getTexture("PNG", ResourceLoader.getResourceAsStream("data/missing.png"))
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
  
  /** Draw the main menu */
  def drawMainMenu() {
    glClear(GL_COLOR_BUFFER_BIT)
    drawQuadTex(background, (width-background.getImageWidth)/2, 
        (height-background.getImageHeight)/2, background.getImageWidth, background.getImageHeight)
    drawQuadTex(continue2, width*17/24, height*1/6, continue.getImageWidth, continue.getImageHeight)
    drawQuadTex(newgame, width*17/24, height*1/6+100, newgame.getImageWidth, newgame.getImageHeight)
    drawQuadTex(quit, width*17/24, height*1/6+200, quit.getImageWidth, quit.getImageHeight)
    fontMenu.drawString(width*17/24, height/10, "Crusader", Color.black)        
    font.drawString(2, height - 24, "Mouse X: " + Mouse.getX.toString, Color.red)
    font.drawString(130, height - 24, "Y: " + (height - Mouse.getY).toString, Color.red)
  }
  
  /** Draw game screen */
  def drawGame() {
    glClear(GL_COLOR_BUFFER_BIT)
    drawQuadTex(tempUIBackground, 0, 0, tempUIBackground.getImageWidth, tempUIBackground.getImageHeight)
    drawlog
    drawSideBar
    drawFloor
    drawObjects
    drawPlayer
    drawFog
    font.drawString(2, 0, Main.player.name + " the Holy", Color.white)
    font.drawString(width - 155, height - 27, "Esc to quit", Color.black)
    font.drawString(2, height - 245, "Mouse X: " + (Mouse.getX * 1.0 / 32 + Main.player.getX - 16).toInt.toString, Color.red)
    font.drawString(130, height - 245, "Y: " + ((height - Mouse.getY) * 1.0 / 32 + Main.player.getY - 8).toInt.toString, Color.red)
    font.drawString(2, height - 225, "Mouse X: " + Mouse.getX.toString, Color.red)
    font.drawString(130, height - 225, "Y: " + (height - Mouse.getY).toString, Color.red)
    font.drawString(2, height - 205, "Version: " + version, Color.white)
    font.drawString(900, height - 205, "Episode: " + Main.episode, Color.white)
    font.drawString(990, height - 205, "Level: " + Main.level, Color.white)
    if (Main.player.health <= 0) fontMenu.drawString(width/4, height/4, "You died!", Color.white)
    
  }
  
  /** Draw gamelog to bottom of the screen */
  def drawlog() {
    val alice = "Alice was beginning to get very tired of sitting by her sister on the bank, and of having nothing to do: once or twice she had peeped into the book her sister was reading, but it had no pictures or conversations in it, 'and what is the use of a book,' thought Alice 'without pictures or conversations?' So she was considering in her own mind (as well as she could, for the hot day made her feel very sleepy and stupid), whether the pleasure of making a daisy-chain would be worth the trouble of getting up and picking the daisies, when suddenly a White Rabbit with pink eyes ran close by her. There was nothing so very remarkable in that; nor did Alice think it so very much out of the way to hear the Rabbit say to itself, 'Oh dear! Oh dear! I shall be late!' (when she thought it over afterwards, it occurred to her that she ought to have wondered at this, but at the time it all seemed quite natural); but when the Rabbit actually took a watch out of its waistcoat-pocket, and looked at it, and then hurried on, Alice started to her feet, for it flashed across her mind that she had never before seen a rabbit with either a waistcoat-pocket, or a watch to take out of it, and burning with curiosity, she ran across the field after it, and fortunately was just in time to see it pop down a large rabbit-hole under the hedge. In another moment down went Alice after it, never once considering how in the world she was to get out again. "
    val tempTextLines = wordWrap(alice, 1030)
    for (line <- Range(0,7)) font.drawString(10, height - 165 + line * 20, tempTextLines(line), Color.black)
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
      if (font.getWidth(line + " " + word) > maxLength) {
        lines += line
        line = word + " "
      }
      else line += word + " "
    }
    lines
  }
  
  /** Draw sidebar to right of the screen */
  def drawSideBar() {
    drawQuadTex(tempHeart, width-239, -25, tempHeart.getImageWidth, tempHeart.getImageHeight)
    font.drawString(width-153, 80, "HP: " + Main.player.health + "/" + Main.player.maxHealth, Color.black)
    drawQuadTex(tempXp, width-239, 100, tempXp.getImageWidth, tempXp.getImageHeight)
    font.drawString(width-153, 210, "XP: " + Main.player.experience + "/10", Color.black)
    font.drawString(width-200, 410, "Items and stuff here", Color.black)
  }
  
  /** Draw player to the middle of screen */
  def drawPlayer() = Main.player.draw
  
  /** Draws Tiles */
  def drawFloor() {
    for (i <- Range(0,33)) {
      for (j <- Range(0,17)) {
        drawQuadTex(tempBlack, i * 32, j * 32, tempBlack.getImageWidth, tempBlack.getImageHeight)
      }
    }
    Main.grid.draw
  }
  
  /** Draw objects */
  def drawObjects() {
    for (monster <- Main.monsterList) 
      if (monster.xDif(Main.player) <= 16 && monster.yDif(Main.player) <= 8) 
        monster.draw
  }
  
  /** Draw fog */
  def drawFog() {
    // not implemented yet
  }
  
}