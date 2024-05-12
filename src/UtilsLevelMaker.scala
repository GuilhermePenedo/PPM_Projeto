import Direction.{Direction, INVALID}
import FileManager.lerPalavrasEscondidas
import UtilsGeneral._

import java.io.{FileNotFoundException, PrintWriter}
import scala.annotation.tailrec


object UtilsLevelMaker {
  case class EditState(board: Board, wordsToAdd: List[(String, List[Coord2D])], colorBoard:Board)
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

  def play(wordInput : String, board: Board, startPos: Coord2D, dir: Direction): Boolean = {
      countPaths(wordInput,board,startPos,dir, Nil) > 0
  }

  def countPaths(wordInput : String, board: Board, startPos: Coord2D, dir: Direction, searchedPos:List[Coord2D]): Int = {
    def isValidPos(pos: Coord2D, board:Board): Boolean = {
      val (i,j) = pos
      i >= 0 && i < board.length && j >= 0 && j < getItem(board, i).length
    }
    val (i1,j1) = startPos
    val (i2, j2) = Direction.nextPos(dir, startPos)
    val wordList = wordInput.toList
    val isCorrectChar = getItem(getItem(board, i1), j1) == getItem(wordList, 0)
    val directionList = Direction.values.toList filter (x => x != INVALID)
    if(isValidPos((i2, j2), board) && isCorrectChar && getItem(getItem(board, i2), j2) == getItem(wordList, 1) && !inList((i2,j2),searchedPos)) {
      searchDirections(wordInput.tail.toList, board, (i2, j2), directionList, searchedPos)
    }else 0
  }

  def searchDirections(word: List[Char], board: Board, startPos: Coord2D, directions: List[Direction], searchedPos:List[Coord2D]): Int = {
  val (i1, j1) = startPos
  val isCorrectChar = getItem (getItem (board, i1), j1) == getItem (word, 0)
  if (word.length == 1) {
    if(isCorrectChar) 1
    else 0
  }else {
    directions match {
        case Nil => 0
        case dir :: tail => countPaths(listToString(word), board, startPos, dir, searchedPos:+startPos) + searchDirections(word, board, startPos, tail, searchedPos:+startPos)
      }
    }
  }


  def checkBoard(board: Board, wordsToFind: List[String]): Boolean = {
    def countOccurrenceInBoard(board: Board, word:List[Char], directions: List[Direction]): Int = {
      def resultRow( current : (Char,Coord2D), res:Int): Int = {
        if(current._1 == word.head) {
          searchDirections(word, board, current._2,directions, Nil) + res
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

  def iniEditState(boardSize: Int, n: String):EditState = {
    try{
      val wordsToPlace = lerPalavrasEscondidas("board" + n+ ".txt")
      val emptyBoard: Board = List.fill(boardSize)(List.fill(boardSize)(' '))
      val boardWithHiddenWords: Board = setBoardWithWords(emptyBoard, wordsToPlace)
      val emptyColorBoard = List.fill(boardSize)(List.fill(boardSize)('W'))
      EditState(boardWithHiddenWords, wordsToPlace, emptyColorBoard)
    }catch {
      case _:FileNotFoundException =>
        new PrintWriter("board" + n + ".txt")
        iniEditState(boardSize, n)
    }
  }

  def updateEditState(oldEditState: EditState, newBoard: Board, newWordsToAdd:List[(String, List[Coord2D])]):EditState = {
    EditState(newBoard, newWordsToAdd,oldEditState.colorBoard)
  }

  def validateBoard(board: Board, wordsToFind: List[(String, List[Coord2D])]): Boolean = {
    val wordList = wordsToFind map (x => x._1)
    checkBoard(board, wordList)
  }
}