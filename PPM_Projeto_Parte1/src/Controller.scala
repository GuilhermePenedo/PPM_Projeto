import Direction.Direction
import FileManager.lerPalavrasEscondidas
import GameEngine.{mainLoop, wordsToFind}
import javafx.fxml.FXML
import javafx.scene.control.{Button, Label, TextField}
import javafx.scene.layout.{ColumnConstraints, GridPane, RowConstraints}
import UtilsGameEngine.{Board, Coord2D, HiddenWord, completeBoardRandomly, getItem, interactWithBoard, iterateBoard, play, randomChar, setBoardWithWords}
import UtilsTUI.{askForWord, getUserInput, printBoard, printGameOver, printGameState, printVictory, reset, showPrompt, wordToColor}
import javafx.event.ActionEvent
import javafx.scene.Node

import scala.annotation.tailrec

case class GUIGameState(board: (Board,MyRandom),
                     wordsToFind: List[HiddenWord],
                     foundedWords: List[HiddenWord],
                     colorBoard:Board)

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
  var plays: List[Coord2D] = _

  @FXML
  var boardPane: GridPane = _

  @FXML
  var makePlayButton: Button = _

  @FXML
  var wordLabel: Label = _

  def getColor(c: Char):String =  c match{
    case 'W' => BUTTON_DEFAULT
    case 'G' => BUTTON_GREEN
  }
  def makeBoard(board: Board, colorBoard:Board):Unit = {
    def aux(c:Char, p:Coord2D):Char = {
      val color = getColor(getItem[Char](getItem[List[Char]](colorBoard, p._1), p._2))
      val button = new Button(c.toString)
      button.setMinWidth(30)
      button.setMinHeight(30)
      button.setOnAction(onLetterClicked)
      button.setStyle(color)
      boardPane.add(button, p._1, p._2)
      c
    }
    interactWithBoard(board, aux)
  }

  def updateBoard(board: Board, colorBoard:Board):Unit = {
    def aux(c:Char, p:Coord2D):Char = {
      val color = getColor(getItem[Char](getItem[List[Char]](colorBoard, p._2), p._1))
      val buttonIndex =  p._1 + p._2 * boardPane.getColumnCount
      val button = boardPane.getChildren.get(buttonIndex)
      button.setStyle(color)
      c
    }
    interactWithBoard(board, aux)
  }

  def initialize(): Unit = {
    val filePath = "HiddenWords.txt"
    val wordsToFind: List[HiddenWord] = lerPalavrasEscondidas(filePath)
    val foundedWords: List[Nothing] = List()
    val numberOfHiddenWords: Int = wordsToFind.length
    val emptyBoard: Board = List.fill(8)(List.fill(8)(' '))
    val emptyColorBoard: Board = List.fill(8)(List.fill(8)('W'))
    val boardWithHiddenWords: Board = setBoardWithWords(emptyBoard, wordsToFind)
    val board: (Board,MyRandom) = completeBoardRandomly(boardWithHiddenWords, MyRandom(2), randomChar)
    val colorBoard: Board = emptyColorBoard

    gameState = GUIGameState(board, wordsToFind, foundedWords, colorBoard)
    word = ""
    plays = Nil
    makeBoard(board._1, colorBoard)
  }

  @FXML
  private def onLetterClicked(event: ActionEvent): Unit = {
    event.getSource match {
      case button: Button =>
        val letter = button.getText
        val x = GridPane.getColumnIndex(button)
        val y = GridPane.getRowIndex(button)
        if(validPlay(x,y, plays) && !inList((x,y), plays)) {
          word += letter
          plays = plays:+(x,y)
          button.setStyle(BUTTON_PRESSED)
        }
      case _ => println("Unknown event source")
    }
  }

  @FXML
  private def onMakePlayButtonClick(): Unit = {
    if(plays.length > 1) {
      val initialCoord = plays.head
      val initialDirection = Direction.getDirection(initialCoord, getItem(plays, 1))
      val found = play(word, initialCoord, initialDirection, gameState.wordsToFind)
      if (found) {
        val foundedHiddenWord = getItem(gameState.wordsToFind.filter(_._1 == word), 0)
        val updatedWordsToFind = gameState.wordsToFind.filterNot(_._1 == word)

        val updatedFoundedWords = gameState.foundedWords :+ foundedHiddenWord

        val foundedWordsColored = updatedFoundedWords map (x => (wordToColor(x._1.toList, "G"), x._2))
        val updatedColorBoard = setBoardWithWords(gameState.colorBoard, foundedWordsColored)

        val newNumWordsRemaining = gameState.wordsToFind.length - 1
        gameState = GUIGameState(gameState.board, updatedWordsToFind, updatedFoundedWords, updatedColorBoard)
        if (newNumWordsRemaining == 0) {
          println("Win")
          resetPlay()
        }

      }
      resetPlay()
    }
  }

  private def resetPlay() = {
    updateBoard(gameState.board._1, gameState.colorBoard)
    word = ""
    plays = Nil
  }

  @FXML
  private def onResetPlayButtonClick(): Unit = {
    resetPlay()
  }



  @tailrec
  private def inList[A](item:A, l:List[A]):Boolean = l match {
    case Nil => false
    case head::tail =>
      if(head == item) true
      else inList(item, tail)
  }

  private def validPlay(x: Integer, y: Integer, plays: List[Coord2D]): Boolean = plays match {
  case Nil => true
  case _ => Direction.getDirection(plays.last, (x, y)) != Direction.Invalid
}
 }