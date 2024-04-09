import FileManager.{NumberOfWordsToFind, lerPalavrasEscondidas}
import Main.{Board, HiddenWord, completeBoardRandomly, play, randomChar, setBoardWithWords}

import scala.annotation.tailrec
import UtilsTUI.{askForWord, getUserInput, printBoard, printGameOver, printGameState, printVictory, showPrompt}

case class GameState(board: (Board,MyRandom), WordsRemainig: Int)

object TUI extends App {

  val board = List.fill(8)(List.fill(8)(' '))

  // Ler palavras escondidas do ficheiro

  val caminhoDoArquivo = "HiddenWords.txt"
  val wordsToFind = lerPalavrasEscondidas(caminhoDoArquivo)

  val board1 = setBoardWithWords(board, wordsToFind)
  val board2 = completeBoardRandomly(board1, MyRandom(2), randomChar)

  val incialGameState = GameState(board2, NumberOfWordsToFind(caminhoDoArquivo))

  mainLoop(incialGameState, wordsToFind)

  @tailrec
  def mainLoop(gameState: GameState, wordsToFind: List[HiddenWord]) {

    showPrompt()
    val userInput = getUserInput()

    // handle the result
    userInput match {

      case "T" => {

        printBoard(gameState.board._1)

        val (word, pos, direction) = askForWord()

        val found = play(word, pos, direction, wordsToFind)

        if (found) {
          // Filtra a palavra encontrada fora de wordsToFind
          val updatedWordsToFind = wordsToFind.filterNot(_._1 == word)

          val newNumWordsRemaining = gameState.WordsRemainig - 1
          val newGameState = gameState.copy(gameState.board, WordsRemainig = newNumWordsRemaining)

          // Verifica se todas as palavras foram encontradas
          if (newGameState.WordsRemainig == 0) {
            printVictory()

          } else {
            printGameState(newGameState)
            mainLoop(newGameState.copy(WordsRemainig = updatedWordsToFind.length), wordsToFind = updatedWordsToFind) // Atualiza o loop com a lista modificada
          }

        } else {
          printGameState(gameState)
          mainLoop(gameState, wordsToFind) // Mantém a lista original, pois a palavra não foi encontrada
        }
      }

      case "Q" | "q"  => {
        printGameOver()
        printGameState(gameState)
        // return out of the recursion here
      }

      case _ => {
        print("Invalid Key")
        mainLoop(gameState, wordsToFind)
      }
    }
  }

}
