import FileManager.{NumberOfWordsToFind, lerPalavrasEscondidas}
import UtilsGameEngine.{Board, HiddenWord, completeBoardRandomly, getItem, play, randomChar, setBoardWithWords}

import scala.annotation.tailrec
import UtilsTUI.{askForWord, getUserInput, printBoard, wordToColor,  printGameOver, printGameState, printVictory, showPrompt}

case class GameState(board: (Board,MyRandom), wordsToFind: List[HiddenWord], foundedWords: List[HiddenWord], colorBoard:Board)

object GameEngine extends App {

  // Ler palavras escondidas do ficheiro
  val filePath = "HiddenWords.txt"
  val wordsToFind = lerPalavrasEscondidas(filePath)
  val foundedWords = List()
  val numberOfHiddenWords = wordsToFind.length

  //Criar tabuleiro de jogo e tabuleiro de cores
  val emptyBoard = List.fill(8)(List.fill(8)(' '))
  val emptyColorBoard = List.fill(8)(List.fill(8)('W'))

  //Colocar as palavras a encontrar e letras aleatórias no tabuleiro
  val boardWithHiddenWords = setBoardWithWords(emptyBoard, wordsToFind)
  val board = completeBoardRandomly(boardWithHiddenWords, MyRandom(2), randomChar)

  // Colocar as cores no tabuleiro de cor
  val colorBoard = emptyColorBoard

  // Criar instancia ininicial de jogo
  val incialGameState = GameState(board, wordsToFind, foundedWords, colorBoard)

  // Iniciar o loop de jogo
  mainLoop(incialGameState)

  @tailrec
  def mainLoop(gameState: GameState) {

    val wordsRemaining = gameState.wordsToFind.length
    showPrompt()
    val userInput = getUserInput()

    // handle the result
    userInput match {

      case "T" => {

        printBoard(gameState.board._1, gameState.colorBoard)

        val (word, pos, direction) = askForWord()

        val found = play(word, pos, direction, gameState.wordsToFind)

        if (found) {
          // Filtra a palavra encontrada fora de wordsToFind
          val foundedHiddenWord = getItem(gameState.wordsToFind.filter(_._1 == word), 0)
          val updatedWordsToFind = gameState.wordsToFind.filterNot(_._1 == word)

          val updatedFoundedWords = gameState.foundedWords :+ foundedHiddenWord

          val foundedWordsColored = updatedFoundedWords map (x => (wordToColor(x._1.toList, "G"), x._2))
          val updatedColorBoard = setBoardWithWords(gameState.colorBoard, foundedWordsColored)

          val newNumWordsRemaining = wordsRemaining - 1
          val newGameState = gameState.copy(gameState.board, updatedWordsToFind, updatedFoundedWords, updatedColorBoard)

          // Verifica se todas as palavras foram encontradas
          if (newNumWordsRemaining == 0) {
            printVictory()

          } else {
            printGameState(newGameState)
            mainLoop(newGameState) // Atualiza o loop com a lista modificada
          }

        } else {
          printGameState(gameState)
          mainLoop(gameState) // Mantém a lista original, pois a palavra não foi encontrada
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
