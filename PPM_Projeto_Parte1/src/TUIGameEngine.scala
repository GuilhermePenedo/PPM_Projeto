import UtilsGameEngine.{GameState, correctGuess, iniGame, play, updateGameState}
import UtilsGeneral.inList

import scala.annotation.tailrec
import UtilsTUI.{askBoardSize, askForPlay, getUserInput, printBoard, printGameOver, printGameState, printVictory, showPrompt, timeRemaining}


object TUIGameEngine extends App {
  val initialTime = System.currentTimeMillis()
  mainLoop(iniGame(askBoardSize()))

  @tailrec
  def mainLoop(gameState: GameState) {
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


