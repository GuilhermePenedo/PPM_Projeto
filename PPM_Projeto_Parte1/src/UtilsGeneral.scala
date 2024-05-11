import scala.annotation.tailrec

object UtilsGeneral{
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
    case Nil => throw new IndexOutOfBoundsException("Nao encontrei elemento -> " + pos.toString + " em " + l.toString())
    case head :: tail => if (pos == 0) head else getItem(tail, pos - 1)
  }
  def getItem[A](matrix: List[List[A]], p: Coord2D): A = getItem(getItem(matrix, p._1),p._2)

  @tailrec
  def inList[A](item:A, l:List[A]): Boolean = l match{
    case Nil => false
    case head::tail =>
      if(item == head) true
      else inList(item, tail)
  }

  @tailrec
  def isPalindrome(s: List[Char]): Boolean = s match{
    case Nil => true
    case _::Nil => true
    case x::xs => if(x == xs.last) isPalindrome(xs.init) else false
  }

  def listToString(l: List[Char]): String = l match {
    case Nil => ""
    case head::tail => head + listToString(tail)
  }

  def nextCoord(pos:Coord2D, board:Board): Coord2D = {
    if(pos._1 < board.length){
      if(pos._2 < board.head.length){(pos._1,pos._2+1)}
      else{(pos._1+1,0)}
    }else{(0,0)}
  }

  def updateMatrix[A](matrix: List[List[A]], fun: (A, Coord2D) => A): List[List[A]] = {

    def updateRow(row: List[A], i: Int): List[A] = {
      def aux(l: List[A], j: Int): List[A] = l match {
        case Nil => Nil
        case head :: tail => fun(head, (i,j)) :: aux(tail, j+1)
      }
      aux(row, 0)
    }

    def aux(m: List[List[A]], p: Coord2D): List[List[A]] = m match {
      case Nil => Nil
      case head :: tail => updateRow(head, p._1) :: aux(tail, (p._1+1, p._2))
    }
    aux(matrix, (0, 0))
  }

  def iterateMatrix[A,B](matrix: List[List[A]], resultM:(B,B) => B, resultRow: ((A, Coord2D),B) => B, emptyValue:B): B= {
    def iterateRow(row: List[A], i: Int): B = {
      def aux(l: List[A], j: Int): B = l match {
        case Nil => emptyValue
        case head :: tail =>
          resultRow((head, (i,j)), aux(tail, j+1))
      }
      aux(row, 0)
    }

    def aux(m: List[List[A]], p: Coord2D): B = m match {
      case Nil => emptyValue
      case head :: tail =>
        resultM(iterateRow(head, p._1),aux(tail, (p._1+1, p._2)))
    }
    aux(matrix, (0, 0))
  }
}
