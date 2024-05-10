import Direction.Direction
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
    case Nil => l.last
    case head :: tail => if (pos == 0) head else getItem(tail, pos - 1)
  }

  @tailrec
  def inList[A](item:A, l:List[A]): Boolean = l match{
    case Nil => false
    case head::tail =>
      if(item == head) true
      else inList(item, tail)
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
      case Nil => List(Nil)
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
      case head::tail => aux(fillOneCell(res, getItem[Char](word.toList, i), head), tail, i + 1)
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
      case Nil => (List(Nil),rAux)
      case head :: tail => {
        val row = iterateRow(head, rAux)
        val nextRow = aux(tail, row._2)
        (row._1 :: nextRow._1, nextRow._2)
      }
    }
    aux(board, r)
  }

  def play(wordInput : String, board: Board, startPos: Coord2D, dir: Direction): Boolean = {
    def isValidMove(pos: Coord2D): Boolean = {
      val (i,j) = pos
      i >= 0 && j < board.length && j >= 0 && j < getItem(board, i).length
    }

    def dfs(word: List[Char], pos: Coord2D, visited: Set[Coord2D], directionList: List[Direction]): Boolean = word match {
      case Nil => false
      case _::tail =>
        serchAllDirections(tail, pos, visited, directionList)
    }

    def serchAllDirections(word: List[Char], pos: Coord2D, visited: Set[Coord2D], directionList: List[Direction]): Boolean = directionList match {
          case Nil => false
          case dir :: dirTail =>
            val nextPos = Direction.nextPos(dir, pos)
            isValidMove(nextPos) &&
            !visited(nextPos) &&
            dfs(word, nextPos, visited + nextPos, dirTail)
    }

    def checkWord(word: List[Char], startPos: Coord2D, dir: Direction): Boolean = {
      val nextPos = Direction.nextPos(dir, startPos)
      if (isValidMove(nextPos) && board(nextPos._1)(nextPos._2) == word(1)) {
        dfs(word, nextPos, Set(startPos, nextPos), Direction.values.toList)
      } else {
        false
      }
    }
    val (i1,j1) = startPos
    val (i2,j2) = Direction.nextPos(dir, startPos)
    val wordList = wordInput.toList
    getItem(getItem(board, i1),j1) == getItem(wordList,0) && getItem(getItem(board, i2),j2) == getItem(wordList,1) && checkWord(wordList.tail, (i2,j2), dir)
  }

  def checkBoard(board: Board, wordsToFind: List[String]): Boolean = {
    def isValidWord(word: String): Boolean =
      word.foldLeft(true)((acc, char) => acc && char.isLetter)

    @tailrec
    def searchFromPositions(positions: List[Coord2D], directions: List[Direction], words: List[String], found: Set[Coord2D]): Boolean = {
      positions match {
        case Nil => true
        case pos :: restPos =>
          directions match {
            case Nil => searchFromPositions(restPos, Direction.values.toList, words, found)
            case dir :: restDir =>
              val foundFromPosition = words.foldLeft(false)((acc, word) => acc || play(word, board, pos, dir))
              if (foundFromPosition) {
                val newPos = Direction.nextPos(dir, pos)
                searchFromPositions(newPos :: restPos, restDir, words, found + pos)
              } else {
                searchFromPositions(restPos, directions, words, found)
              }
          }
      }
    }

    val positions: List[Coord2D] =
      board.zipWithIndex.foldLeft(List.empty[Coord2D]) {
        case (acc, (row, i)) =>
          row.zipWithIndex.foldLeft(acc) {
            case (innerAcc, (_, j)) => (i, j) :: innerAcc
          }
      }

    val result = searchFromPositions(positions, Direction.values.toList, wordsToFind, Set())

    val allWordsValid = wordsToFind.foldLeft(true)((acc, word) => acc && isValidWord(word))

    result && allWordsValid
  }


}