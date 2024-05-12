import UtilsGameEngine.{GameState, correctGuess, iniGame, updateGameState}
import scala.annotation.tailrec
import UtilsTUI.{askBoardID, askBoardSize, askForPlay, getUserInput, printBoard, printGameOver, printGameState, printVictory, showPrompt, timeRemaining}


object TUIGameEngine extends App {
  startGame()
  @tailrec
  private def startGame():Unit = {
    try {
      mainLoop(iniGame(askBoardSize(), askBoardID()))
    }catch {
      case _:IllegalArgumentException =>
        println("No board with that ID, try again")
        startGame()
    }
  }

  @tailrec
  private def mainLoop(gameState: GameState): Unit = {
    if(gameState.timeout - System.currentTimeMillis() > 0) {
      timeRemaining(gameState.timeout)
      showPrompt()
      val input = getUserInput()
      if(gameState.timeout - System.currentTimeMillis() > 0) {
        input match {
          case "T" | "t" =>
            printBoard(gameState.board._1, gameState.colorBoard)
            val (word, initialCoord, initialDirection) = askForPlay()
            if (correctGuess(word, initialCoord, initialDirection, gameState)){
              val newGameState = updateGameState(gameState, word)
              if (newGameState.wordsToFind.isEmpty) {
                printGameState(newGameState)
                printVictory(newGameState.timeout)
              } else {
                printGameState(newGameState)
                mainLoop(newGameState)

              }
            } else {
              printGameState(gameState)
              mainLoop(gameState)
            }

          case "Q" | "q" =>
            printGameState(gameState)
            printGameOver("You quited")

          case _ =>
            println("Invalid Key")
            mainLoop(gameState)
        }
        }else{
          printGameState(gameState)
          printGameOver("Ran out of time...")
        }
      } else{
        printGameState(gameState)
        printGameOver("Ran out of time...")
      }
    }
}


