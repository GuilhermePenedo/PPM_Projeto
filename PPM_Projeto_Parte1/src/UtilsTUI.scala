import Direction._
import UtilsGameEngine.GameState
import UtilsGeneral.{Board, Coord2D, getItem, iterateMatrix, updateMatrix}

import scala.annotation.tailrec
import scala.io.StdIn.{readInt, readLine}

object UtilsTUI {
  val RESET = "\u001B[0m"
  val PROMPT_MESSAGE = "\n(T)ry word or (q)uit: "
  val INVALID_KEY_MESSAGE = "Invalid Key"
  val WORD_INPUT_MESSAGE = "What word do you want to input: "
  val COORDINATES_INPUT_MESSAGE = "What are the coordinates (line,column): "
  val DIRECTION_INPUT_MESSAGE = "What is the initial direction? (Ex. North, SouthEast, West): "
  val GAME_OVER_MESSAGE = "\n=== GAME OVER ===\n"
  val VICTORY_MESSAGE = "\n=== CONGRATULATIONS ==="
  val ASK_BOARD_SIZE = "What is the size of the board you want to play with?: "

  def showPrompt(): Unit = {
    print(PROMPT_MESSAGE)
  }

  def askBoardSize(): Int = {
    print(ASK_BOARD_SIZE)
    val input = readLine()
    try {
      input.toInt
    } catch {
      case _: NumberFormatException =>
        println("Invalid input. Please enter a valid number.")
        askBoardSize() // Recursive call
    }
  }

  def printBoard(board: Board, colorBoard: Board): Unit = {
    def getColor(c: Char): String = c match {
      case 'W' => "\u001B[0m"
      case 'G' => "\u001B[32m"
    }
    def resRow(current:(Char,Coord2D), a:String):String = {
      val colorType = getItem(colorBoard, current._2)
      val color = getColor(colorType)
      color + current._1.toString + RESET + " " + a
    }
    print("\n" + iterateMatrix(board, (a:String,b:String)=>(a + "\n" + b),resRow, "", "") + "\n")
  }

  def wordToColor(w: List[Char], c: String): String = w match {
    case Nil => ""
    case head :: tail => wordToColor(tail, c) + c
  }

  @tailrec
  def setGreenWords(colorBoard: Board, coords: List[Coord2D]): Board = {
    def aux(colorBoard: Board, coord: Coord2D): Board = {
      def checkCell(c: Char, p: Coord2D): Char = {
        if (p == coord) 'G' else c
      }
      updateMatrix(colorBoard, checkCell)
    }
    coords match{
      case Nil => colorBoard
      case head::tail => setGreenWords(aux(colorBoard, head), tail)
    }
  }

  def getUserInput(): String = readLine.trim.toUpperCase

  def stringToDirection(s: String): Direction = s.toUpperCase match {
    case "NORTH" => NORTH
    case "SOUTH" => SOUTH
    case "EAST" => EAST
    case "WEST" => WEST
    case "NORTHEAST" => NORTHEAST
    case "NORTHWEST" => NORTHWEST
    case "SOUTHEAST" => SOUTHEAST
    case "SOUTHWEST" => SOUTHWEST
    case _ => throw new IllegalArgumentException("Invalid direction.")
  }

  def stringToPoint(s: String): Coord2D = {
    val Array(x, y) = s.split(",").map(_.toInt)
    (x, y)
  }

  def askForPlay(): (String, Coord2D, Direction) = {
    (askWord(), askPoint(), askDirection())
  }

  def askWord(): String = {
    print(WORD_INPUT_MESSAGE)
    readLine().toUpperCase()
  }

  def askPoint(): Coord2D = {
    print(COORDINATES_INPUT_MESSAGE)
    val input = readLine()
    try {
      stringToPoint(input)
    } catch {
      case _: NumberFormatException | _: ArrayIndexOutOfBoundsException =>
        println("Invalid input format. Please enter coordinates in the format 'line,column'.")
        askPoint() // Recursive call
    }
  }

  def askDirection(): Direction = {
    print(DIRECTION_INPUT_MESSAGE)
    val input = readLine().toUpperCase()
    try {
      stringToDirection(input)
    } catch {
      case _: IllegalArgumentException =>
        println("Invalid direction. Please enter a valid direction.")
        askDirection() // Recursive call
    }
  }

  def printGameState(gameState: GameState): Unit = {
    printBoard(gameState.board._1, gameState.colorBoard)
    println("Words Remaining: " + gameState.wordsToFind.length)
  }


  def timeRemaining(timeout:Long): Unit = {
    val timeLeft = (timeout - System.currentTimeMillis())/1000
    println("Time Left: " + timeLeft.toString + "s, hurry up!")
  }
  def printGameOver(reason: String): Unit = println(GAME_OVER_MESSAGE + reason)

  def printVictory(timeout: Long): Unit = {
    val timeLeft = (timeout - System.currentTimeMillis())/1000
    println("Wow, you still had " + timeLeft.toString + "s to spare!\n" + VICTORY_MESSAGE)
  }
}
