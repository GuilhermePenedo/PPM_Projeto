import Direction.{Direction, INVALID}
import FileManager.{lerPalavrasEscondidas, readRandom, writeRandom}
import UtilsGeneral._
import UtilsTUI.setGreenWords

import scala.annotation.tailrec


object UtilsGameEngine {
  case class GameState(board: (Board,MyRandom), wordsToFind: List[(String, List[Coord2D])], colorBoard:Board, timeout:Long)
  val DEFAULT_TIMEOUT = 60
  def fillOneCell(board:Board, letter: Char, coord:Coord2D):Board = {
    def checkCell(c: Char, p: Coord2D): Char = {
      if (p == coord) letter else c
    }

    updateMatrix(board, checkCell)
  }

  def fillWord(board:Board, word:String, position:List[Coord2D]): Board = {
    @tailrec
    def aux(res:Board, positionAux:List[Coord2D], i:Int): Board = positionAux match {
      case Nil => res
      case head::tail => aux(fillOneCell(res, getItem(word.toList, i), head), tail, i + 1)
    }

    aux(board, position, 0)
  }

  def setBoardWithWords(board:Board, hiddenWords: List[(String, List[Coord2D])]): Board = {
    @tailrec
    def aux(res:Board, hiddenWordsAux:List[(String, List[Coord2D])]): Board = hiddenWordsAux match{
      case Nil => res
      case (hWord, hPos)::tail => {
        aux(fillWord(res, hWord,hPos), tail)
      }
    }
    aux(board, hiddenWords)
  }

  def completeBoardRandomly(board:Board,r:MyRandom, f: MyRandom => (Char, MyRandom)):(Board, MyRandom) = {
    def iterateRow(row:List[Char], rowR:MyRandom): (List[Char],MyRandom)  = {
      def aux(l: List[Char], rAux:MyRandom): (List[Char],MyRandom) = l match {
        case Nil => (Nil,rAux)
        case head :: tail => {
          val currentR = f(rAux)
          val nextCell = aux(tail, currentR._2)
          if(head == ' '){
            (currentR._1 :: nextCell._1, nextCell._2)
          } else (head :: nextCell._1, nextCell._2)
        }
      }
      aux(row, rowR)
    }

    def aux(bAux:Board, rAux:MyRandom):(Board,MyRandom) = bAux match {
      case Nil => (Nil,rAux)
      case head :: tail => {
        val row = iterateRow(head, rAux)
        val nextRow = aux(tail, row._2)
        (row._1 :: nextRow._1, nextRow._2)
      }
    }
    aux(board, r)
  }

  def play(wordInput : String, board: Board, startPos: Coord2D, dir: Direction): Boolean = {
      countPaths(wordInput : String, board: Board, startPos: Coord2D, dir: Direction) > 0
  }

  def correctGuess(word: String, initialCoord:Coord2D, initialDirection:Direction, gameState:GameState):Boolean = {
    play(word, gameState.board._1, initialCoord, initialDirection) && inList(word, gameState.wordsToFind map (x => x._1))
  }

  def countPaths(wordInput : String, board: Board, startPos: Coord2D, dir: Direction): Int = {
    def isValidPos(pos: Coord2D, board:Board): Boolean = {
      val (i,j) = pos
      i >= 0 && i < board.length && j >= 0 && j < getItem(board, i).length
    }

    val (i1,j1) = startPos
    val (i2, j2) = Direction.nextPos(dir, startPos)
    val wordList = wordInput.toList
    val isCorrectChar = getItem(getItem(board, i1), j1) == getItem(wordList, 0)
    val directionList = Direction.values.toList filter (x => x != Direction.getOpposite(dir) &&  x != INVALID)
    if(isValidPos((i2, j2), board) && isCorrectChar && getItem(getItem(board, i2), j2) == getItem(wordList, 1)) {
      searchDirections(wordInput.tail.toList, board, (i2, j2), directionList)
    }else 0
  }

  def searchDirections(word: List[Char], board: Board, startPos: Coord2D, directions: List[Direction]): Int = {
  val (i1, j1) = startPos
  val isCorrectChar = getItem (getItem (board, i1), j1) == getItem (word, 0)
  if (word.length == 1) {
    if(isCorrectChar) 1
    else 0
  }else {
    directions match {
        case Nil => 0
        case dir :: tail => countPaths(listToString(word), board, startPos, dir) + searchDirections(word, board, startPos, tail)
      }
    }
  }


  def checkBoard(board: Board, wordsToFind: List[String]): Boolean = {
    def countOccurrenceInBoard(board: Board, word:List[Char], directions: List[Direction]): Int = {
      def resultRow( current : (Char,Coord2D), res:Int): Int = {
        if(current._1 == word.head) {
          searchDirections(word, board, current._2,directions) + res
        }else {
          res
        }
      }
      iterateMatrix(board, (a:Int, b:Int) => (a+b) , resultRow, 0, 0)

    }

    val directions = Direction.values.toList filter (x => x != INVALID)
    val correctNrWord = wordsToFind map (x => if(isPalindrome(x.toList)) 2 else 1)
    val wordsOccurrences = wordsToFind map (x => (countOccurrenceInBoard(board, x.toList, directions)))
    correctNrWord == wordsOccurrences
  }

  def iniGame(boardSize: Int):GameState = {
    val seed = readRandom()
    val wordsToPlace = lerPalavrasEscondidas("HiddenWords.txt")
    val emptyBoard: Board = List.fill(boardSize)(List.fill(boardSize)(' '))
    val boardWithHiddenWords: Board = setBoardWithWords(emptyBoard, wordsToPlace)
    val board: (Board,MyRandom) = validateBoard(completeBoardRandomly(boardWithHiddenWords, MyRandom(seed), randomChar), wordsToPlace)
    writeRandom(board._2.nextInt._1)
    val emptyColorBoard = List.fill(boardSize)(List.fill(boardSize)('W'))
    GameState(board, wordsToPlace, emptyColorBoard, System.currentTimeMillis() + (DEFAULT_TIMEOUT*1000))
  }

  def updateGameState(oldGameState: GameState, guess:String):GameState = {
    val foundedCoords = getItem(oldGameState.wordsToFind.filter(_._1 == guess), 0)._2 //Sabendo que existe uma palavra certa na posiçao indicada verifica se foi a palavra dada
    val updatedWordsToFind = oldGameState.wordsToFind.filterNot(_._1 == guess)
    val updatedColorBoard = setGreenWords(oldGameState.colorBoard, foundedCoords)

    GameState(oldGameState.board, updatedWordsToFind,updatedColorBoard, oldGameState.timeout)
  }

  def validateBoard(boardR: (Board,MyRandom), wordsToFind: List[(String, List[Coord2D])]): (Board, MyRandom) = {
    val wordList = wordsToFind map (x => x._1)
    val boardSize = boardR._1.length
    val r = boardR._2
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