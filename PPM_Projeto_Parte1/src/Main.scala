import scala.annotation.tailrec

object Main {
  type Board = List[List[Char]]
  type Coord2D = (Int, Int)
  def randomChar(rand:MyRandom):(Char, MyRandom) = {
    val r = rand.nextInt
    // A = 65  e Z = 90
    val n = (r._1 % (25))
    val c = (if(n<0) (-n + 65) else (n + 65))
    (c.toChar, MyRandom(r._1))
  }

  def fillOneCell(board:Board, letter: Char, coord:Coord2D):Board = {
    def aux(row: List[Char], x: Int): List[Char] = row match {
      case Nil => Nil
      case head :: tail => if (x == 0) letter :: tail else head :: aux(tail, x - 1)
    }
    board match {
      case List(Nil) => List(Nil)
      case head :: tail => if (coord._2 == 0) aux(head, coord._1) :: tail else head :: fillOneCell(board, letter, (coord._1, coord._2 - 1))
    }
  }
  def getItem[A](l:List[A], pos:Int): A = l match{
    case Nil => l.last
    case head::tail => if(pos == 0) head else getItem(tail, pos-1)
  }

  def fillWord(board:Board, word:String, position:List[Coord2D]): Board = {
    @tailrec
    def aux(res:Board, positionAux:List[Coord2D], i:Int): Board = positionAux match {
      case Nil => res
      case head::tail => aux(fillOneCell(res, getItem[Char](word.toList, i), head), tail, i + 1)
    }
    aux(board, position, 0)
  }

  def setBoardWithWords(board:Board, words:List[String], positions:List[List[Coord2D]]): Board = {
    @tailrec
    def aux(res:Board, positionsAux:List[List[Coord2D]], i:Int): Board = positionsAux match{
      case Nil => res
      case head::tail => {
        System.out.println(res)
        System.out.println(getItem[String](words, i), head)
        aux(fillWord(res, getItem[String](words, i), head), tail, i + 1)
      }
    }
    aux(board, positions, 0)
  }



  def main(args: Array[String]): Unit = {
    val board = List.fill(8)(List.fill(8)('A'))
    val words = List("Ola", "Bau", "Pim")
    val board1 = setBoardWithWords(board, words, List(List((0,0),(1,0),(2,0)),List((0,1),(1,1),(2,1)),List((0,2),(1,2),(2,2))))
    System.out.println(board1)

  }
}