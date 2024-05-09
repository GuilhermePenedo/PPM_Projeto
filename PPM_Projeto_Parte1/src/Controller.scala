import Direction.Direction
import FileManager.lerPalavrasEscondidas
import GameEngine.mainLoop
import javafx.fxml.FXML
import javafx.scene.control.{Button, Label, TextField}
import javafx.scene.layout.{ColumnConstraints, GridPane, RowConstraints}
import UtilsGameEngine.{Board, Coord2D, HiddenWord, completeBoardRandomly, getItem, interactWithBoard, iterateBoard, play, randomChar, setBoardWithWords}
import UtilsTUI.{askForWord, getUserInput, printBoard, printGameOver, printGameState, printVictory, showPrompt, wordToColor}
import javafx.event.ActionEvent
import javafx.scene.Node

import scala.annotation.tailrec

case class GUIGameState(board: (Board,MyRandom),
                     wordsToFind: List[HiddenWord],
                     foundedWords: List[HiddenWord],
                     colorBoard:Board,
                     play:List[(Char,Coord2D)],
                     playOver: Boolean)

class Controller {

  val BUTTON_PRESSED = "-fx-background-color: #BEBEFFFF;"
  val BUTTON_DEFAULT = "-fx-background-color: #ffffff;"
  // Ler palavras escondidas do ficheiro
  val filePath = "HiddenWords.txt"
  val wordsToFind: List[HiddenWord] = lerPalavrasEscondidas(filePath)
  val foundedWords: List[Nothing] = List()
  val numberOfHiddenWords: Int = wordsToFind.length

  //Criar tabuleiro de jogo e tabuleiro de cores
  val emptyBoard: Board = List.fill(8)(List.fill(8)(' '))
  val emptyColorBoard: Board = List.fill(8)(List.fill(8)('W'))

  //Colocar as palavras a encontrar e letras aleatórias no tabuleiro
  val boardWithHiddenWords: Board = setBoardWithWords(emptyBoard, wordsToFind)
  val board: (Board,MyRandom) = completeBoardRandomly(boardWithHiddenWords, MyRandom(2), randomChar)

  // Colocar as cores no tabuleiro de cor
  val colorBoard: Board = emptyColorBoard

  // Criar instancia ininicial de jogo
  val initialGUIGameState: GUIGameState = GUIGameState(board, wordsToFind, foundedWords, colorBoard, List(), playOver = false)

  @FXML
  var grid1: GridPane = _

  @FXML
  var button1: Button = _

  @FXML
  var wordLabel: Label = _

  def updateBoard(board: Board):Unit = {
    def aux(c:Char, p:Coord2D):Char = {
      val button = new Button(c.toString)
      button.setMinWidth(30)
      button.setMinHeight(30)
      button.setOnAction(onButtonClicked)
      button.setStyle(BUTTON_DEFAULT)
      grid1.add(button, p._2, p._1)
      c
    }
    interactWithBoard(board, aux)
  }

  @FXML
  private def onButtonClicked(event: ActionEvent): Unit = {
    event.getSource match {
      case button: Button =>
          button.setStyle(BUTTON_PRESSED)
      case _ =>
        println("Unknown event source")
    }
  }

  def mainLoop(gameState: GUIGameState): Unit = {
    updateBoard(gameState.board._1)
    //val userInput = getUserInput()
    val wordsRemaining = gameState.wordsToFind.length
    //val currentPlay = getLastPlay(gameState.board._1, gameState.play)
    //println(currentPlay)

    // handle the result
    if(gameState.playOver){
        val word, pos, direction = getInfoFromPlay(gameState.play)
        val found = play(word, pos, direction, gameState.wordsToFind)

        if (found) {
          // Filtra a palavra encontrada fora de wordsToFind
          val foundedHiddenWord = getItem(gameState.wordsToFind.filter(_._1 == word), 0)
          val updatedWordsToFind = gameState.wordsToFind.filterNot(_._1 == word)

          val updatedFoundedWords = gameState.foundedWords :+ foundedHiddenWord

          val foundedWordsColored = updatedFoundedWords map (x => (wordToColor(x._1.toList, "G"), x._2))
          val updatedColorBoard = setBoardWithWords(gameState.colorBoard, foundedWordsColored)

          val newNumWordsRemaining = wordsRemaining - 1
          val newGUIGameState = gameState.copy(gameState.board, updatedWordsToFind, updatedFoundedWords, updatedColorBoard)

          // Verifica se todas as palavras foram encontradas
          if (newNumWordsRemaining == 0) {
            printVictory()

          }

        }
      }
    //mainLoop(gameState)
 }

  private def getLastPlay(board: Board, oldPlays: List[(Char, Coord2D)]) : (Char, Coord2D) = {
    def aux(c:Char, p:Coord2D):Boolean = {
      val button: Node = grid1.getChildren.get(p._1 + p._2 * grid1.getRowCount)
      !inList[(Char, Coord2D)]((c,p), oldPlays) && button.getStyle == BUTTON_PRESSED
    }

    val (found, result) = iterateBoard(board, aux)
    if(found) result else getLastPlay(board: Board, oldPlays: List[(Char, Coord2D)]) : (Char, Coord2D)


  }

  @tailrec
  private def inList[A](item: A, l: List[A]): Boolean = l match {
    case Nil => true
    case h::tail => if(h == item) inList(item,tail) else false
  }


  private def getInfoFromPlay(play: List[(Char, (Int, Int))]) = ???
}
