import UtilsLevelMaker._
import UtilsGeneral._
import javafx.event.ActionEvent
import javafx.fxml.FXML
import javafx.scene.control.{Button, Label, TextField}
import javafx.scene.layout.GridPane

import java.io.{File, FileNotFoundException, PrintWriter}

class LevelMakerController {
  val BUTTON_BORDER = "-fx-border-color: black; " + "-fx-border-width: 1px; " + "-fx-border-style: solid;"
  val BUTTON_PRESSED = "-fx-background-color: #BEBEFFFF;" + BUTTON_BORDER
  val BUTTON_DEFAULT = "-fx-background-color: #ffffff;" + BUTTON_BORDER
  val BUTTON_GREEN = "-fx-background-color: #4CBB17;" + BUTTON_BORDER

  // Criar instancia ininicial de jogo
  var editState: EditState = _
  var word: String = _
  var coords: List[Coord2D] = _
  var boardID: String = ""

  @FXML
  var boardPane: GridPane = _

  @FXML
  var addButton: Button = _

  @FXML
  var resetButton: Button = _

  @FXML
  var saveButton: Button = _

  @FXML
  var changeBoardButton: Button = _

  @FXML
  var wordInput: TextField = _

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


  def startEdit(boardID: String): Unit = {
    editState = iniEditState(5, boardID)
    boardPane.getChildren.clear()
    boardPane.setVisible(true)
    makeBoard(editState.board)
    paintBoard(editState.board, editState.colorBoard)
    resetSelection()
  }

  @FXML
  private def onLetterClicked(event: ActionEvent): Unit = {
    event.getSource match {
      case button: Button =>
        val letter = button.getText
        val x = GridPane.getRowIndex(button)
        val y = GridPane.getColumnIndex(button)
        if(validPlay(x,y, coords) && !inList((x,y), coords)) {
          word += letter
          coords = coords:+(x,y)
          button.setStyle(BUTTON_PRESSED)
        }
      case _ => println("Unknown event source")
    }
  }

  @FXML
  private def onAddButtonClick(): Unit = {
    val toAddWord = wordInput.getText.toUpperCase
    if(toAddWord.length == word.length && overlap(toAddWord.toList, word.toList)) {
      val newHiddenWords = editState.wordsToAdd :+ (toAddWord, coords)
      val newBoard = setBoardWithWords(editState.board, newHiddenWords)
      if(validateBoard(newBoard, newHiddenWords)){
        editState = updateEditState(editState, newBoard, newHiddenWords)
      }
    }
    paintBoard(editState.board, editState.colorBoard)
    resetSelection()
  }

  @FXML
  private def onResetSelectionButtonClick(): Unit = {
    paintBoard(editState.board, editState.colorBoard)
    resetSelection()
  }
  @FXML
  private def onSaveButtonClick(): Unit = {
    val writer = new PrintWriter("board" + boardID + ".txt")
    try {
        writer.print(wordsToAddToFile(editState.wordsToAdd))
    } finally {
      writer.close()
    }
  }

  private def wordsToAddToFile(wordsToAdd: List[(String, List[(Int, Int)])]):String = wordsToAdd match{
    case Nil => ""
    case head::tail => head._1 +","+ head._2.toString().replace(" ", "") + "\n" + wordsToAddToFile(tail)
  }

  @FXML
  private def onChangeBoardButtonClick(): Unit = {
    val newBoardID = boardIDInput.getText
    if(boardID != newBoardID)
      boardID = newBoardID
      startEdit(boardID)
  }

  def overlap(a: List[Char], b: List[Char]):Boolean = {
    a match{
      case Nil => true
      case head::tail => ((head == b.head) || (b.head == ' ')) && overlap(tail, b.tail)
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
      button match {
        case x:Button => x.setText(getItem(board, current._2).toString)
      }
      button.setStyle(getSyle(styleType))
    }
    def resM(a:Unit, b:Unit):Unit = {}
    iterateMatrix[Char, Unit, Unit](board, resM, resRow, (), ())
  }

  private def resetSelection() = {
    wordInput.setText("")
    word = ""
    coords = Nil
  }

  private def validPlay(x: Integer, y: Integer, guess: List[Coord2D]): Boolean = guess match {
    case Nil => true
    case _ => Direction.getDirection(guess.last, (x, y)) != Direction.INVALID
  }

}