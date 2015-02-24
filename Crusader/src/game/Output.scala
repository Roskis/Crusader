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
  private var continue:Texture = null
  private var continue2:Texture = null
  private var newgame:Texture = null
  private var quit:Texture = null
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
    
    background = loadTexture("tempBackground")
    continue = loadTexture("UI/Continue")
    continue2 = loadTexture("UI/Continue2")
    newgame = loadTexture("UI/Newgame")
    quit = loadTexture("UI/Quit")
    unseen = loadTexture("Tiles/unseen")
    tempHeart = loadTexture("tempHeart")
    tempXp = loadTexture("tempXp")
    tempUIBackground = loadTexture("tempUIBackground")
    
    addLog("Alice was beginning to get very tired of sitting by her sister on the bank, and of having nothing to do: once or twice she had peeped into the book her sister was reading, but it had no pictures or conversations in it, 'and what is the use of a book,' thought Alice 'without pictures or conversations?' So she was considering in her own mind (as well as she could, for the hot day made her feel very sleepy and stupid), whether the pleasure of making a daisy-chain would be worth the trouble of getting up and picking the daisies, when suddenly a White Rabbit with pink eyes ran close by her. There was nothing so very remarkable in that; nor did Alice think it so very much out of the way to hear the Rabbit say to itself, 'Oh dear! Oh dear! I shall be late!' (when she thought it over afterwards, it occurred to her that she ought to have wondered at this, but at the time it all seemed quite natural); but when the Rabbit actually took a watch out of its waistcoat-pocket, and looked at it, and then hurried on, Alice started to her feet, for it flashed across her mind that she had never before seen a rabbit with either a waistcoat-pocket, or a watch to take out of it, and burning with curiosity, she ran across the field after it, and fortunately was just in time to see it pop down a large rabbit-hole under the hedge. In another moment down went Alice after it, never once considering how in the world she was to get out again. The rabbit-hole went straight on like a tunnel for some way, and then dipped suddenly down, so suddenly that Alice had not a moment to think about stopping herself before she found herself falling down a very deep well. Either the well was very deep, or she fell very slowly, for she had plenty of time as she went down to look about her and to wonder what was going to happen next. First, she tried to look down and make out what she was coming to, but it was too dark to see anything; then she looked at the sides of the well, and noticed that they were filled with cupboards and book-shelves; here and there she saw maps and pictures hung upon pegs. She took down a jar from one of the shelves as she passed; it was labelled 'ORANGE MARMALADE', but to her great disappointment it was empty: she did not like to drop the jar for fear of killing somebody, so managed to put it into one of the cupboards as she fell past it. 'Well!' thought Alice to herself, 'after such a fall as this, I shall think nothing of tumbling down stairs! How brave they'll all think me at home! Why, I wouldn't say anything about it, even if I fell off the top of the house!' (Which was very likely true.) Down, down, down. Would the fall never come to an end! 'I wonder how many miles I've fallen by this time?' she said aloud. 'I must be getting somewhere near the centre of the earth. Let me see: that would be four thousand miles down, I think—' (for, you see, Alice had learnt several things of this sort in her lessons in the schoolroom, and though this was not a very good opportunity for showing off her knowledge, as there was no one to listen to her, still it was good practice to say it over) '—yes, that's about the right distance—but then I wonder what Latitude or Longitude I've got to?' (Alice had no idea what Latitude was, or Longitude either, but thought they were nice grand words to say.) Presently she began again. 'I wonder if I shall fall right through the earth! How funny it'll seem to come out among the people that walk with their heads downward! The Antipathies, I think—' (she was rather glad there was no one listening, this time, as it didn't sound at all the right word) '—but I shall have to ask them what the name of the country is, you know. Please, Ma'am, is this New Zealand or Australia?' (and she tried to curtsey as she spoke—fancy curtseying as you're falling through the air! Do you think you could manage it?) 'And what an ignorant little girl she'll think me for asking! No, it'll never do to ask: perhaps I shall see it written up somewhere.' Down, down, down. There was nothing else to do, so Alice soon began talking again. 'Dinah'll miss me very much to-night, I should think!' (Dinah was the cat.) 'I hope they'll remember her saucer of milk at tea-time. Dinah my dear! I wish you were down here with me! There are no mice in the air, I'm afraid, but you might catch a bat, and that's very like a mouse, you know. But do cats eat bats, I wonder?' And here Alice began to get rather sleepy, and went on saying to herself, in a dreamy sort of way, 'Do cats eat bats? Do cats eat bats?' and sometimes, 'Do bats eat cats?' for, you see, as she couldn't answer either question, it didn't much matter which way she put it. She felt that she was dozing off, and had just begun to dream that she was walking hand in hand with Dinah, and saying to her very earnestly, 'Now, Dinah, tell me the truth: did you ever eat a bat?' when suddenly, thump! thump! down she came upon a heap of sticks and dry leaves, and the fall was over. Alice was not a bit hurt, and she jumped up on to her feet in a moment: she looked up, but it was all dark overhead; before her was another long passage, and the White Rabbit was still in sight, hurrying down it. There was not a moment to be lost: away went Alice like the wind, and was just in time to hear it say, as it turned a corner, 'Oh my ears and whiskers, how late it's getting!' She was close behind it when she turned the corner, but the Rabbit was no longer to be seen: she found herself in a long, low hall, which was lit up by a row of lamps hanging from the roof. There were doors all round the hall, but they were all locked; and when Alice had been all the way down one side and up the other, trying every door, she walked sadly down the middle, wondering how she was ever to get out again. Suddenly she came upon a little three-legged table, all made of solid glass; there was nothing on it except a tiny golden key, and Alice's first thought was that it might belong to one of the doors of the hall; but, alas! either the locks were too large, or the key was too small, but at any rate it would not open any of them. However, on the second time round, she came upon a low curtain she had not noticed before, and behind it was a little door about fifteen inches high: she tried the little golden key in the lock, and to her great delight it fitted! Alice opened the door and found that it led into a small passage, not much larger than a rat-hole: she knelt down and looked along the passage into the loveliest garden you ever saw. How she longed to get out of that dark hall, and wander about among those beds of bright flowers and those cool fountains, but she could not even get her head through the doorway; 'and even if my head would go through,' thought poor Alice, 'it would be of very little use without my shoulders. Oh, how I wish I could shut up like a telescope! I think I could, if I only knew how to begin.' For, you see, so many out-of-the-way things had happened lately, that Alice had begun to think that very few things indeed were really impossible. There seemed to be no use in waiting by the little door, so she went back to the table, half hoping she might find another key on it, or at any rate a book of rules for shutting people up like telescopes: this time she found a little bottle on it, ('which certainly was not here before,' said Alice,) and round the neck of the bottle was a paper label, with the words 'DRINK ME' beautifully printed on it in large letters. It was all very well to say 'Drink me,' but the wise little Alice was not going to do that in a hurry. 'No, I'll look first,' she said, 'and see whether it's marked \"poison\" or not'; for she had read several nice little histories about children who had got burnt, and eaten up by wild beasts and other unpleasant things, all because they would not remember the simple rules their friends had taught them: such as, that a red-hot poker will burn you if you hold it too long; and that if you cut your finger very deeply with a knife, it usually bleeds; and she had never forgotten that, if you drink much from a bottle marked 'poison,' it is almost certain to disagree with you, sooner or later. However, this bottle was not marked 'poison,' so Alice ventured to taste it, and finding it very nice, (it had, in fact, a sort of mixed flavour of cherry-tart, custard, pine-apple, roast turkey, toffee, and hot buttered toast,) she very soon finished it off. 'What a curious feeling!' said Alice; 'I must be shutting up like a telescope.' And so it was indeed: she was now only ten inches high, and her face brightened up at the thought that she was now the right size for going through the little door into that lovely garden. First, however, she waited for a few minutes to see if she was going to shrink any further: she felt a little nervous about this; 'for it might end, you know,' said Alice to herself, 'in my going out altogether, like a candle. I wonder what I should be like then?' And she tried to fancy what the flame of a candle is like after the candle is blown out, for she could not remember ever having seen such a thing. After a while, finding that nothing more happened, she decided on going into the garden at once; but, alas for poor Alice! when she got to the door, she found she had forgotten the little golden key, and when she went back to the table for it, she found she could not possibly reach it: she could see it quite plainly through the glass, and she tried her best to climb up one of the legs of the table, but it was too slippery; and when she had tired herself out with trying, the poor little thing sat down and cried. 'Come, there's no use in crying like that!' said Alice to herself, rather sharply; 'I advise you to leave off this minute!' She generally gave herself very good advice, (though she very seldom followed it), and sometimes she scolded herself so severely as to bring tears into her eyes; and once she remembered trying to box her own ears for having cheated herself in a game of croquet she was playing against herself, for this curious child was very fond of pretending to be two people. 'But it's no use now,' thought poor Alice, 'to pretend to be two people! Why, there's hardly enough of me left to make one respectable person!' Soon her eye fell on a little glass box that was lying under the table: she opened it, and found in it a very small cake, on which the words 'EAT ME' were beautifully marked in currants. 'Well, I'll eat it,' said Alice, 'and if it makes me grow larger, I can reach the key; and if it makes me grow smaller, I can creep under the door; so either way I'll get into the garden, and I don't care which happens!' She ate a little bit, and said anxiously to herself, 'Which way? Which way?', holding her hand on the top of her head to feel which way it was growing, and she was quite surprised to find that she remained the same size: to be sure, this generally happens when one eats cake, but Alice had got so much into the way of expecting nothing but out-of-the-way things to happen, that it seemed quite dull and stupid for life to go on in the common way. So she set to work, and very soon finished off the cake. ")
    
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
    if (getPlayer.health <= 0) fontMenu.drawString(width/4, height/4, "You died! Score: " + getPlayer.gold, Color.white)
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
    font.drawString(width-153, 80, "HP: " + getPlayer.health + "/" + getPlayer.maxHealth, Color.black)
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