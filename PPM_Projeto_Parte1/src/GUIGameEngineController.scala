import UtilsGameEngine.{GameState, correctGuess, iniGame, updateGameState}
import UtilsGeneral.{Board, Coord2D, getItem, inList, iterateMatrix}
import javafx.animation.AnimationTimer
import javafx.fxml.FXML
import javafx.scene.control.{Button, Label, TextField}
import javafx.scene.layout.GridPane
import javafx.event.ActionEvent

class GUIGameEngineController{
  val BUTTON_BORDER = "-fx-border-color: black; " + "-fx-border-width: 1px; " + "-fx-border-style: solid;"
  val BUTTON_PRESSED = "-fx-background-color: #BEBEFFFF;" + BUTTON_BORDER
  val BUTTON_DEFAULT = "-fx-background-color: #ffffff;" + BUTTON_BORDER
  val BUTTON_GREEN = "-fx-background-color: #4CBB17;" + BUTTON_BORDER

  var gameState: GameState = _
  var word: String = _
  var guess: List[Coord2D] = _
  var boardID: String = "0"
  var timer: Timer = _

  @FXML
  var boardPane: GridPane = _

  @FXML
  var makePlayButton: Button = _

  @FXML
  var resetPlayButton: Button = _

  @FXML
  var timerLabel: Label = _

  @FXML
  var endGameLabel: Label = _

  @FXML
  var changeBoardButton: Button = _

  @FXML
  var boardIDInput: TextField = _

  def makeBoard(board: Board):Unit = {
    def resRow(current:(Char,Coord2D), a:Unit):Unit = {
      val button = new Button(current._1.toString)
      button.setMinWidth(45)
      button.setMinHeight(45)
      button.setOnAction(onLetterClicked)
      button.setStyle(BUTTON_DEFAULT)
      boardPane.add(button, current._2._2, current._2._1)
    }
    def resM(a:Unit, b:Unit):Unit = {}
    iterateMatrix[Char, Unit, Unit](board, resM, resRow, (),())
  }

  def initialize(): Unit = {
    boardIDInput.setText(boardID)
    startGame()
  }

  def startGame(): Unit = {
    gameState = iniGame(5, boardID)
    boardPane.getChildren.clear()
    boardPane.setVisible(true)
    makePlayButton.setVisible(true)
    resetPlayButton.setVisible(true)
    endGameLabel.setVisible(false)
    makeBoard(gameState.board._1)
    paintBoard(gameState.board._1, gameState.colorBoard)

    timer = new Timer(this)
    timer.startTimer()
    resetPlay()
  }

  @FXML
  private def onLetterClicked(event: ActionEvent): Unit = {
    event.getSource match {
      case button: Button =>
        val letter = button.getText
        val x = GridPane.getRowIndex(button)
        val y = GridPane.getColumnIndex(button)
        if(validPlay(x,y, guess) && !inList((x,y), guess)) {
          word += letter
          guess = guess:+(x,y)
          button.setStyle(BUTTON_PRESSED)
        }
      case _ => println("Unknown event source")
    }
  }
  @FXML
  private def onMakePlayButtonClick(): Unit = {
    if(guess.length > 1) {
      val initialCoord = guess.head
      val initialDirection = Direction.getDirection(initialCoord, getItem(guess, 1))
      if (correctGuess(word, initialCoord, initialDirection, gameState)) {
        gameState = updateGameState(gameState, word)
        paintBoard(gameState.board._1, gameState.colorBoard)
        resetPlay()
        if (gameState.wordsToFind.isEmpty) {
          victory()
        }

      }else{
        paintBoard(gameState.board._1, gameState.colorBoard)
      }
      resetPlay()
    }
  }

  @FXML
  private def onResetPlayButtonClick(): Unit = {
    paintBoard(gameState.board._1, gameState.colorBoard)
    resetPlay()
  }

  @FXML
  private def onRestartButtonClick(): Unit = {
    timer.stopTimer()
    startGame()
  }

  @FXML
  private def onChangeBoardButtonClick(): Unit = {
    timer.stopTimer()
    val newBoardID = boardIDInput.getText
    if(boardID != newBoardID)
      boardID = newBoardID
      startGame()
  }

  private def paintBoard(board: Board, colorBoard: Board):Unit = {
    def getSyle(c: Char): String = c match {
      case 'W' => BUTTON_DEFAULT
      case 'G' => BUTTON_GREEN
    }
    def resRow(current:(Char,Coord2D), a:Unit):Unit = {
      val buttonIndex =  boardPane.getRowCount * (current._2._1 + 1) - current._2._2 -1
      val button = boardPane.getChildren.get(buttonIndex)
      val styleType = getItem(colorBoard, current._2)
      button.setStyle(getSyle(styleType))
    }
    def resM(a:Unit, b:Unit):Unit = {}
    iterateMatrix[Char, Unit, Unit](board, resM, resRow, (), ())
  }

  private def resetPlay() = {
    word = ""
    guess = Nil
  }

  private def validPlay(x: Integer, y: Integer, guess: List[Coord2D]): Boolean = guess match {
    case Nil => true
    case _ => Direction.getDirection(guess.last, (x, y)) != Direction.INVALID
  }


  def endGame():Unit = {
    timer.stopTimer()
    boardPane.setVisible(false)
    makePlayButton.setVisible(false)
    resetPlayButton.setVisible(false)
    endGameLabel.setVisible(true)
  }
  def gameOver(reason: String): Unit = {
    endGame()
    endGameLabel.setText(endMessage(gameState, reason))
  }

  def victory(): Unit = {
    endGame()
    endGameLabel.setText(endMessage(gameState, ""))
  }

  private def endMessage(gameState: GameState, reason: String):String = {
      val timeLeft = (gameState.timeout - System.currentTimeMillis())/1000
      gameState.wordsToFind.length match{
      case 0 =>
        "Congrats, you won with " + timeLeft + "s left"
      case x =>
          "You lost because " + reason + ", there were " + x.toString + " words left to find"
    }
  }

}

class Timer(controller: GUIGameEngineController) extends AnimationTimer {
  val timeout = controller.gameState.timeout
  var label = controller.timerLabel
  private val clockSymbols = Array(
    "\uD83D\uDD5B", "\uD83D\uDD5A", "\uD83D\uDD59", "\uD83D\uDD58", "\uD83D\uDD57", "\uD83D\uDD56", "\uD83D\uDD55", "\uD83D\uDD54", "\uD83D\uDD53", "\uD83D\uDD52", "\uD83D\uDD51", "\uD83D\uDD50"
  )
  override def handle(now: Long): Unit = {
    val timeLeft = (timeout - System.currentTimeMillis()) / 1000
    if(timeLeft > 0){
      val index = (timeLeft % clockSymbols.length).toInt
      label.setText(clockSymbols(index) + " " + timeLeft.toString)
    }else{
      label.setText(clockSymbols(0) + " " + 0)
      controller.gameOver("time ran out")
      stopTimer()
    }
  }

  def startTimer(): Unit = {
    this.start()
  }

  def stopTimer(): Unit = {
    this.stop()
  }
}