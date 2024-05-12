import javafx.application.Application
import javafx.fxml.{FXML, FXMLLoader}
import javafx.scene.{Parent, Scene}
import javafx.stage.Stage


class GUIGameEngine extends Application {
  override def start(primaryStage: Stage): Unit = {
    primaryStage.setTitle("Letter Soup")
    val fxmlLoader =
      new FXMLLoader(getClass.getResource("GUIGameEngineController.fxml"))
    val mainViewRoot: Parent = fxmlLoader.load()
    val scene = new Scene(mainViewRoot)
    primaryStage.setScene(scene)
    primaryStage.show()
  }
}
object GUIGameEngineApp {
  def main(args: Array[String]): Unit = {
    Application.launch(classOf[GUIGameEngine], args: _*)
  }
}
