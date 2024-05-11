import Direction.{Direction, INVALID}
import FileManager.lerPalavrasEscondidas

import scala.annotation.tailrec

object UtilsGameEngine {
  type Board = List[List[Char]]
  type Coord2D = (Int, Int)


  def randomChar(rand:MyRandom):(Char, MyRandom) = {
    val r = rand.nextInt
    // A = 65  e Z = 90
    val n = (r._1 % (25))
    val c = (if(n<0) (-n + 65) else (n + 65))
    (c.toChar, MyRandom(r._1))
  }
  @tailrec
  def getItem[A](l: List[A], pos: Int): A = l match {
    case Nil => throw new IndexOutOfBoundsException("Nao encontrei elemento")
    case head :: tail => if (pos == 0) head else getItem(tail, pos - 1)
  }

  @tailrec
  def inList[A](item:A, l:List[A]): Boolean = l match{
    case Nil => false
    case head::tail =>
      if(item == head) true
      else inList(item, tail)
  }

  def compCoord(a:Coord2D, b:Coord2D, board: Board): Int = {
    val index1 = a._1 + a._2 * getItem(board,a._1).length
    val index2 = b._1 + b._2 * getItem(board,b._1).length
    index1-index2
  }

  def nextCoord(pos:Coord2D, board:Board): Coord2D = {
    if(pos._1 < board.length){
      if(pos._2 < board.head.length){
        (pos._1,pos._2+1)
      }else{
        println(pos._2)
        (pos._1+1,0)
      }
    }else{
      (0,0)
    }
  }
  def iterateBoard(board: Board, fun: (Char, Coord2D) => Boolean): (Boolean, (Char, Coord2D)) = {

    def iterateRow(row: List[Char], i: Int): (Boolean, (Char, Coord2D)) = {
      def aux(l: List[Char], j: Int): (Boolean, (Char, Coord2D)) = l match {
        case Nil => (false, (' ', (i, j)))
        case head :: Nil => (false, (head, (i, j)))
        case head :: tail =>
          if (fun(head, (i, j))) (true, (head, (i, j)))
          else aux(tail, j + 1)
      }
      aux(row, 0)
    }

    def aux(bAux: Board, p: Coord2D): (Boolean, (Char, Coord2D)) = bAux match {
      case Nil => (false, (' ', p))
      case head :: tail =>
        val (found, result) = iterateRow(head, p._1)
        if (found) (found, result)
        else aux(tail, (p._1 + 1,0))
    }
    aux(board, (0, 0))
  }
  def interactWithBoard(board:Board, fun: (Char,Coord2D) => Char):Board = {

    def interactWithRow(row:List[Char], i:Int): List[Char] = {
      def aux(l: List[Char], j:Int): List[Char] = l match {
        case Nil => Nil
        case head :: tail => fun(head, (i,j)) :: aux(tail, (j+1))
      }
      aux(row,0)
    }

    def aux(bAux:Board, p:Coord2D):Board = bAux match {
      case Nil => Nil
      case head :: tail => interactWithRow(head,p._1) :: aux(tail, (p._1+1,0))
    }
    aux(board, (0,0))
  }

  def fillOneCell(board:Board, letter: Char, coord:Coord2D):Board = {
    def checkCell(c: Char, p: Coord2D): Char = {
      if (p == coord) letter else c
    }

    interactWithBoard(board, checkCell)
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


  private def listToString(l: List[Char]): String = l match {
    case Nil => ""
    case head::tail => head + listToString(tail)
  }

  def countPaths(wordInput : String, board: Board, startPos: Coord2D, dir: Direction): Int = {
    def isValidPos(pos: Coord2D, board:Board): Boolean = {
      val (i,j) = pos
      i >= 0 && j < board.length && j >= 0 && j < getItem(board, i).length
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


  def checkBoard(board: Board, wordsToFind: List[String]): List[Int] = {
    def countOccurrenceInBoard(board: Board, word:List[Char], directions: List[Direction]): Int = {
      def countOccurrenceInRow(row: List[Char], i: Int): Int = {
        def aux(l: List[Char], j: Int): Int = l match {
          case Nil => 0
          case head :: tail=>
            if(head == word.head) {
              searchDirections(word, board, (i,j),directions) + aux(tail, j + 1)
            }else {
              aux(tail, j + 1)
            }
        }
        aux(row, 0)
      }

      def aux(bAux: Board, p: Coord2D): Int = bAux match {
        case Nil => 0
        case head :: tail =>
          countOccurrenceInRow(head, p._1) + aux(tail, (p._1 + 1,0))
      }
      aux(board, (0, 0))
    }

    val directions = Direction.values.toList filter (x => x != INVALID)
    wordsToFind map (x => (countOccurrenceInBoard(board, x.toList, directions)))
  }
  





}