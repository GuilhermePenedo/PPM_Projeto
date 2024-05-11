import Direction.Direction
import FileManager.lerPalavrasEscondidas
import javafx.fxml.FXML
import javafx.scene.control.{Button, Label, TextField}
import javafx.scene.layout.{GridPane}
import UtilsGameEngine.{Board, Coord2D, completeBoardRandomly, getItem, interactWithBoard, play, randomChar, setBoardWithWords, inList}
import javafx.event.ActionEvent
import javafx.scene.Node

import scala.annotation.tailrec

case class GUIGameState(board: (Board,MyRandom),
                     wordsToFind: List[String])

case class Guess(stage: Int,
                 word: String,
                 initialCoord: Coord2D,
                 dir: Direction)

class Controller {
  val BUTTON_BORDER = "-fx-border-color: black; " + "-fx-border-width: 1px; " + "-fx-border-style: solid;"
  val BUTTON_PRESSED = "-fx-background-color: #BEBEFFFF;" + BUTTON_BORDER
  val BUTTON_DEFAULT = "-fx-background-color: #ffffff;" + BUTTON_BORDER
  val BUTTON_GREEN = "-fx-background-color: #00FF00;" + BUTTON_BORDER

  // Criar instancia ininicial de jogo
  var gameState: GUIGameState = _
  var word: String = _
  var guess: List[Coord2D] = _
  var greenSquares: List[Coord2D] = _

  @FXML
  var boardPane: GridPane = _

  @FXML
  var makePlayButton: Button = _

  @FXML
  var wordLabel: Label = _

  def makeBoard(board: Board):Unit = {
    def aux(c:Char, p:Coord2D):Char = {
      val button = new Button(c.toString)
      button.setMinWidth(30)
      button.setMinHeight(30)
      button.setOnAction(onLetterClicked)
      button.setStyle(BUTTON_DEFAULT)
      boardPane.add(button, p._2, p._1)
      c
    }
    interactWithBoard(board, aux)
  }

  def paintBoard(board: Board, coords:List[Coord2D], color: String):Boolean = coords match {
    case Nil => true
    case (x,y)::tail =>
      val buttonIndex =  y + x * boardPane.getRowCount
      val button = boardPane.getChildren.get(buttonIndex)
      //if(!inList((x,y), greenSquares)){
        button.setStyle(color)
      //}
      paintBoard(board, tail, color)
  }

  def initialize(): Unit = {
    val filePath = "HiddenWords.txt"
    val wordsToPlace = lerPalavrasEscondidas(filePath)
    val wordsToFind =  wordsToPlace map (x => x._1)
    val emptyBoard: Board = List.fill(8)(List.fill(8)(' '))
    val boardWithHiddenWords: Board = setBoardWithWords(emptyBoard, wordsToPlace)
    val board: (Board,MyRandom) = completeBoardRandomly(boardWithHiddenWords, MyRandom(2), randomChar)

    gameState = GUIGameState(board, wordsToFind)
    resetPlay()
    makeBoard(board._1)
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
      val found = play(word, gameState.board._1, initialCoord, initialDirection) && inList(word, gameState.wordsToFind)
      println(gameState.wordsToFind)
      println(word, initialCoord, initialDirection)
      if (found) {
        //greenSquares = guess
        paintBoard(gameState.board._1, guess, BUTTON_GREEN)
        val updatedWordsToFind = gameState.wordsToFind.filterNot(_ == word)

        gameState = GUIGameState(gameState.board, updatedWordsToFind)
        if (gameState.wordsToFind.isEmpty) {
          println("Win")
          resetPlay()
        }

      }else{
        paintBoard(gameState.board._1, guess, BUTTON_DEFAULT)
      }
      resetPlay()
    }
  }

  private def resetPlay() = {
    word = ""
    guess = Nil
  }

  @FXML
  private def onResetPlayButtonClick(): Unit = {
    paintBoard(gameState.board._1, guess, BUTTON_DEFAULT)
    resetPlay()
  }

  private def validPlay(x: Integer, y: Integer, guess: List[Coord2D]): Boolean = guess match {
    case Nil => true
    case _ => Direction.getDirection(guess.last, (x, y)) != Direction.INVALID
  }

 }