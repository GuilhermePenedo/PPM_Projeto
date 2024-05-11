import FileManager.{NumberOfWordsToFind, lerPalavrasEscondidas}
import UtilsGameEngine.{Board, Coord2D, checkBoard, completeBoardRandomly, getItem, play, randomChar, setBoardWithWords}

import scala.annotation.tailrec
import UtilsTUI.{askForPlay, getUserInput, printBoard, printGameOver, printGameState, printVictory, showPrompt, wordToColor, askBoardSize}

case class GameState(board: (Board,MyRandom), wordsToFind: List[(String, List[Coord2D])], foundedWords: List[(String, List[Coord2D])], colorBoard:Board)

object TUIGameEngine extends App {


  val boardSize = askBoardSize()
  // Ler palavras escondidas do ficheiro
  val filePath = "HiddenWords.txt"
  val wordsToFind = lerPalavrasEscondidas(filePath) // List de tuplos com a palavra escondida e as coordenadas


  val foundedWords = List() //List que aramzena as palavras encontradas

  val numberOfHiddenWords = wordsToFind.length

  //Criar tabuleiro de jogo e tabuleiro de cores
  val emptyBoard = List.fill(boardSize)(List.fill(boardSize)(' '))
  val emptyColorBoard = List.fill(boardSize)(List.fill(boardSize)('W'))

  //Colocar as palavras a encontrar e letras aleatórias no tabuleiro
  val boardWithHiddenWords = setBoardWithWords(emptyBoard, wordsToFind)
  val board = validateBoard(completeBoardRandomly(boardWithHiddenWords, MyRandom(2), randomChar), wordsToFind)

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
5
          printBoard(gameState.board._1, gameState.colorBoard)
          val (word, pos, direction) = askForPlay()
          val found = play(word, gameState.board._1, pos, direction) //Valida apenas se da posiçao dada existe uma palvra que está no ficheiro

          if (found) {
            // Filtra a palavra encontrada fora de wordsToFind
            println("Play = true")
            val foundedHiddenWord = getItem(gameState.wordsToFind.filter(_._1 == word), 0) //Sabendo que existe uma palavra certa na posiçao indicada verifica se foi a palavra dada
            val updatedWordsToFind = gameState.wordsToFind.filterNot(_._1 == word)

            val updatedFoundedWords = gameState.foundedWords :+ foundedHiddenWord

            val foundedWordsColored = updatedFoundedWords map (x => (wordToColor(x._1.toList, "G"), x._2))
            val updatedColorBoard = setBoardWithWords(gameState.colorBoard, foundedWordsColored)

            val newNumWordsRemaining = wordsRemaining - 1
            val newGameState = gameState.copy(gameState.board, updatedWordsToFind, updatedFoundedWords, updatedColorBoard)

            // Verifica se todas as palavras foram encontradas
            if (newNumWordsRemaining == 0) {
              printGameState(newGameState)
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

        case "Q" | "q" => {
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
  @tailrec
  def validateBoard(boardR: (Board,MyRandom), wordsToFind: List[(String, List[Coord2D])]): (Board, MyRandom) = {
    val wordList = wordsToFind map (x => x._1)
    val boardSize = boardR._1.length
    val r = boardR._2

    printBoard(boardR._1, List.fill(boardSize)(List.fill(boardSize)('W')))
    if (checkBoard(boardR._1, wordList)) {
      boardR
    } else {
      val emptyBoard = List.fill(boardSize)(List.fill(boardSize)(' '))
      val boardWithHiddenWords = setBoardWithWords(emptyBoard, wordsToFind)
      val boardR = completeBoardRandomly(boardWithHiddenWords, r, randomChar)
      validateBoard(boardR, wordsToFind)
    }
  }
}


