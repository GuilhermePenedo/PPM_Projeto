import javafx.animation.AnimationTimer
import javafx.application.Application
import javafx.fxml.{FXML, FXMLLoader}
import javafx.scene.{Parent, Scene}
import javafx.stage.Stage


class GameGUI extends Application {
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
