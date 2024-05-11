import UtilsGameEngine.{GameState, correctGuess, iniGame, play, updateGameState}
import UtilsGeneral.inList

import scala.annotation.tailrec
import UtilsTUI.{askBoardSize, askForPlay, getUserInput, printBoard, printGameOver, printGameState, printVictory, showPrompt}


object TUIGameEngine extends App {
  mainLoop(iniGame(askBoardSize()))

  @tailrec
  def mainLoop(gameState: GameState) {
    showPrompt()
    getUserInput() match {
      case "T" | "t" =>
        printBoard(gameState.board._1, gameState.colorBoard)
        val (word, initialCoord, initialDirection) = askForPlay()
        if (correctGuess(word, initialCoord, initialDirection, gameState)){
          val newGameState = updateGameState(gameState, word)
          if (newGameState.wordsToFind.isEmpty) {
            printGameState(newGameState)
            printVictory()
          } else {
            printGameState(newGameState)
            mainLoop(newGameState)

          }
        } else {
          printGameState(gameState)
          mainLoop(gameState)
        }

      case "Q" | "q" =>
        printGameOver()
        printGameState(gameState)

      case _ =>
        print("Invalid Key")
        mainLoop(gameState)
    }
  }
}


