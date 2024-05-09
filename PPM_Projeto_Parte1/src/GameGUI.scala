import FileManager.lerPalavrasEscondidas
import UtilsGameEngine.{Board, HiddenWord, completeBoardRandomly, randomChar, setBoardWithWords}
import javafx.animation.AnimationTimer
import javafx.application.Application
import javafx.fxml.{FXML, FXMLLoader}
import javafx.scene.control.Button
import javafx.scene.layout.GridPane
import javafx.scene.{Node, Parent, Scene}
import javafx.stage.Stage


class GameGUI extends Application {

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
  override def start(primaryStage: Stage): Unit = {
    primaryStage.setTitle("Letter Soup")
    val fxmlLoader =
      new FXMLLoader(getClass.getResource("Controller.fxml"))
    val mainViewRoot: Parent = fxmlLoader.load()
    val scene = new Scene(mainViewRoot)
    primaryStage.setScene(scene)
    primaryStage.show()
  }
}
object FxApp {
  def main(args: Array[String]): Unit = {
    Application.launch(classOf[GameGUI], args: _*)
  }
}
