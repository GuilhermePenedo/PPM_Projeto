import UtilsGameEngine.{GameState, correctGuess, iniGame, play, updateGameState}
import UtilsGeneral.{Board, Coord2D, getItem, inList, iterateMatrix}
import UtilsTUI.printBoard
import javafx.fxml.FXML
import javafx.scene.control.{Button, Label}
import javafx.scene.layout.GridPane
import javafx.event.ActionEvent

class Controller {
  val BUTTON_BORDER = "-fx-border-color: black; " + "-fx-border-width: 1px; " + "-fx-border-style: solid;"
  val BUTTON_PRESSED = "-fx-background-color: #BEBEFFFF;" + BUTTON_BORDER
  val BUTTON_DEFAULT = "-fx-background-color: #ffffff;" + BUTTON_BORDER
  val BUTTON_GREEN = "-fx-background-color: #4CBB17;" + BUTTON_BORDER

  // Criar instancia ininicial de jogo
  var gameState: GameState = _
  var word: String = _
  var guess: List[Coord2D] = _
  var greenSquares: List[Coord2D] = Nil

  @FXML
  var boardPane: GridPane = _

  @FXML
  var makePlayButton: Button = _

  @FXML
  var wordLabel: Label = _

  def makeBoard(board: Board):Unit = {
    def resRow(current:(Char,Coord2D), a:Unit):Unit = {
      val button = new Button(current._1.toString)
      button.setMinWidth(30)
      button.setMinHeight(30)
      button.setOnAction(onLetterClicked)
      button.setStyle(BUTTON_DEFAULT)
      boardPane.add(button, current._2._2, current._2._1)
    }
    def resM(a:Unit, b:Unit):Unit = {}

    iterateMatrix[Char, Unit, Unit](board, resM, resRow, (),())
  }

  def initialize(): Unit = {
    gameState = iniGame(5)
    makeBoard(gameState.board._1)
    paintBoard(gameState.board._1, gameState.colorBoard)
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
      println(word, gameState.board._1, initialCoord, initialDirection)
      if (correctGuess(word, initialCoord, initialDirection, gameState)) {
        gameState = updateGameState(gameState, word)
        paintBoard(gameState.board._1, gameState.colorBoard)
        resetPlay()
        if (gameState.wordsToFind.isEmpty) {
          println("Win")
        }

      }else{
        paintBoard(gameState.board._1, gameState.colorBoard)
      }
      resetPlay()
    }
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

    printBoard(colorBoard, colorBoard)
    iterateMatrix[Char, Unit, Unit](board, resM, resRow, (), ())
  }

  private def resetPlay() = {
    word = ""
    guess = Nil
  }

  @FXML
  private def onResetPlayButtonClick(): Unit = {
    paintBoard(gameState.board._1, gameState.colorBoard)
    resetPlay()
  }

  private def validPlay(x: Integer, y: Integer, guess: List[Coord2D]): Boolean = guess match {
    case Nil => true
    case _ => Direction.getDirection(guess.last, (x, y)) != Direction.INVALID
  }

 }