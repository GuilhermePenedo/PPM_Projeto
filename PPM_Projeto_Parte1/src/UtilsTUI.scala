import Direction.{Direction, East, North, NorthEast, NorthWest, South, SouthEast, SouthWest, West}
import UtilsGameEngine.{Board, Coord2D, HiddenWord, getItem, iterateBoard}

import scala.io.StdIn.readLine

object UtilsTUI {
  val reset = "\u001B[0m"
  def showPrompt(): Unit = {
    print("\n(T)ry word or (q)uit: ")
  }
  def printBoard(board: Board, colorBoard:Board):Unit = {
    def getColor(c: Char):String =  c match{
      case 'W' => "\u001B[0m"
      case 'G' => "\u001B[32m"
    }
    def printChar(c:Char, p:Coord2D):Char = {
      val colorType = getItem[Char](getItem[List[Char]](colorBoard, p._2), p._1)
      val color = getColor(colorType)
      if(p._1 == 0){System.out.print("\n")}
      System.out.print(color + c.toString + reset + " ")
      c
    }
    iterateBoard(board, printChar)
  }

  def wordToColor(w:List[Char], c:String):String = w match {
    case Nil => ""
    case head::tail => wordToColor(tail, c) + c
  }

  def getUserInput(): String = readLine.trim.toUpperCase


  def stringToDirection(s: String): Direction = s match {
    case "North" => Direction.North
    case "South" => Direction.South
    case "East" => Direction.East
    case "West" => Direction.West
    case "NorthEast" => Direction.NorthEast
    case "NorthWest" => Direction.NorthWest
    case "SouthEast" => Direction.SouthEast
    case "SouthWest" => Direction.SouthWest
  }

  def stringToPoint(s: String): Coord2D = {
    val Array(x, y) = s.split(",").map(_.toInt)
    (x, y)
  }

  def askForWord(): (String, Coord2D, Direction.Value) = {

    println("\nTry (word:initialCoordinate/i.e->0,0:direction)")

    val Array(word, pointStr, directionStr) = readLine().split(":")
    val point = stringToPoint(pointStr)
    val direction = stringToDirection(directionStr)

    (word, point, direction)

  }

  def printGameState(gameState: GameState): Unit = {
    printBoard(gameState.board._1, gameState.colorBoard)
    println("\n Words Remainig: " + gameState.wordsToFind.length)
  }

  def printGameOver(): Unit = println("\n=== GAME OVER ===")

  def printVictory(): Unit = println("\n=== CONGRATULATIONS ===")


}
