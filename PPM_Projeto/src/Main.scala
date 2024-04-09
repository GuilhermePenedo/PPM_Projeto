import Direction.Direction
import FileManager.lerPalavrasEscondidas

import scala.annotation.tailrec

object Main {
  type Board = List[List[Char]]
  type Coord2D = (Int, Int)
  type HiddenWord = (String, List[Coord2D])


  def randomChar(rand:MyRandom):(Char, MyRandom) = {
    val r = rand.nextInt
    // A = 65  e Z = 90
    val n = (r._1 % (25))
    val c = (if(n<0) (-n + 65) else (n + 65))
    (c.toChar, MyRandom(r._1))
  }
  def getItem[A](l:List[A], pos:Int): A = l match{
    case Nil => l.last
    case head::tail => if(pos == 0) head else getItem(tail, pos-1)
  }
  def iterateBoard(board:Board, fun: (Char,Coord2D) => Char):Board = {

    def iterateRow(row:List[Char], y:Int): List[Char] = {
      def aux(l: List[Char], x:Int): List[Char] = l match {
        case Nil => Nil
        case head :: tail => fun(head, (x,y)) :: aux(tail, (x+1))
      }
      aux(row,0)
    }

    def aux(bAux:Board, p:Coord2D):Board = bAux match {
      case Nil => List(Nil)
      case head :: tail => iterateRow(head,p._2) :: aux(tail, (0, p._2+1))
    }
    aux(board, (0,0))
  }

  def fillOneCell(board:Board, letter: Char, coord:Coord2D):Board = {
    def checkCell(c: Char, p: Coord2D): Char = {
      if (p == coord) letter else c
    }

    iterateBoard(board, checkCell)
  }

  def fillWord(board:Board, word:String, position:List[Coord2D]): Board = {
    @tailrec
    def aux(res:Board, positionAux:List[Coord2D], i:Int): Board = positionAux match {
      case Nil => res
      case head::tail => aux(fillOneCell(res, getItem[Char](word.toList, i), head), tail, i + 1)
    }

    aux(board, position, 0)
  }

  def setBoardWithWords(board:Board, hiddenWords: List[HiddenWord]): Board = {
    @tailrec
    def aux(res:Board, hiddenWordsAux:List[HiddenWord]): Board = hiddenWordsAux match{
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

  def play(word:String, pos: Coord2D, dir:Direction, wordsToFind:List[HiddenWord]):Boolean = wordsToFind match{
    case Nil => false
    case (hWord, hPos)::tail => if(word == hWord && getItem[Coord2D](hPos, 0) == pos && getItem[Coord2D](hPos, 1) == Direction.nextPos(dir, pos)) true else play(word, pos, dir, tail)
  }

  def TUI(): Unit = {

  }

  def main(args: Array[String]): Unit = {

//    val board = List.fill(8)(List.fill(8)(' '))
//
////    val hiddenWord1 = ("OLA",List((0,0),(0,1),(0,2)))
////    val hiddenWord2 = ("BAU",List((1,0),(1,1),(1,2)))
////    val hiddenWord3 = ("Pim",List((2,0),(2,1),(2,2)))
////    val wordsToFind = List(hiddenWord1)
//
//    // Ler palavras escondidas do ficheiro
//    val caminhoDoArquivo = "HiddenWords.txt"
//    val wordsToFind = lerPalavrasEscondidas(caminhoDoArquivo)
//
//    val board1 = setBoardWithWords(board, wordsToFind)
//    val board2 = completeBoardRandomly(board1, MyRandom(2), randomChar)
//    printBoard(board2._1)
//    System.out.println(play("Pim", (2,0),Direction.South, wordsToFind))

  }
}