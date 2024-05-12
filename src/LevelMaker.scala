import javafx.application.Application
import javafx.fxml.FXMLLoader
import javafx.scene.{Parent, Scene}
import javafx.stage.Stage


class LevelMaker extends Application {
  override def start(primaryStage: Stage): Unit = {
    primaryStage.setTitle("Level Maker")
    val fxmlLoader =
      new FXMLLoader(getClass.getResource("LevelMakerController.fxml"))
    val mainViewRoot: Parent = fxmlLoader.load()
    val scene = new Scene(mainViewRoot)
    primaryStage.setScene(scene)
    primaryStage.show()
  }
}
object LevelMakerApp {
  def main(args: Array[String]): Unit = {
    Application.launch(classOf[LevelMaker], args: _*)
  }
}
