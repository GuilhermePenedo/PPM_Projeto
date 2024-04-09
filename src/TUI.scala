import FileManager.{NumberOfWordsToFind, lerPalavrasEscondidas}
import Main.{Board, completeBoardRandomly, play, randomChar, setBoardWithWords}

import scala.annotation.tailrec
import UtilsTUI.{askForWord, getUserInput, printBoard, printGameOver, printGameState, printNewGame, showPrompt}

case class GameState(board: (Board,MyRandom), WordsRemainig: Int)

object TUI extends App {

  val board = List.fill(8)(List.fill(8)(' '))

  // Ler palavras escondidas do ficheiro

  val caminhoDoArquivo = "HiddenWords.txt"
  val wordsToFind = lerPalavrasEscondidas(caminhoDoArquivo)

  val board1 = setBoardWithWords(board, wordsToFind)
  val board2 = completeBoardRandomly(board1, MyRandom(2), randomChar)

  val incialGameState = GameState(board2, NumberOfWordsToFind(caminhoDoArquivo))

  mainLoop(incialGameState)

  @tailrec
  def mainLoop(gameState: GameState) {

    showPrompt()
    val userInput = getUserInput()

    // handle the result
    userInput match {

      case "T" => {

        printBoard(gameState.board._1)

        val (word, pos, direction) = askForWord()

        val found = play(word, pos, direction, wordsToFind)

        if (found) {


          val newNumWordsRemaining = gameState.WordsRemainig - 1
          val newGameState = gameState.copy(gameState.board, WordsRemainig = newNumWordsRemaining)
          printGameState(newGameState)
          mainLoop(newGameState)

        } else {

          printGameState(gameState)
          mainLoop(gameState)

        }
      }

      case "Q" | "q"  => {
        printGameOver()
        printGameState(gameState)
        // return out of the recursion here
      }

      case _ => {
        print("Invalid Key")
        mainLoop(gameState)
      }
    }
  }

}
