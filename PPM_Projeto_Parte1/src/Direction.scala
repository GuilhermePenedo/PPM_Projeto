import UtilsGameEngine.Coord2D

object Direction extends Enumeration {
  type Direction = Value
  val NORTH, SOUTH, EAST, WEST,
  NORTHEAST, NORTHWEST, SOUTHEAST, SOUTHWEST, INVALID = Value


  def nextPos(direction: Direction, pos: Coord2D):Coord2D = direction match{
    case NORTH  => (pos._1 - 1, pos._2)
    case SOUTH => (pos._1 + 1, pos._2)
    case EAST => (pos._1, pos._2 + 1)
    case WEST => (pos._1, pos._2 - 1)
    case NORTHEAST => (pos._1 - 1, pos._2 + 1)
    case NORTHWEST => (pos._1 - 1, pos._2 - 1)
    case SOUTHEAST => (pos._1 + 1, pos._2 + 1)
    case SOUTHWEST => (pos._1 + 1, pos._2 - 1)
  }

  def getDirection(iCoord: Coord2D, fCoord: Coord2D):Direction = {
    val dX = iCoord._1 - fCoord._1
    val dY = iCoord._2 - fCoord._2

    (dX, dY) match {
      case (1, 0) => NORTH
      case (-1, 0) => SOUTH
      case (0, -1) => EAST
      case (0, 1) => WEST
      case (1, -1) => NORTHEAST
      case (1, 1) => NORTHWEST
      case (-1, -1) => SOUTHEAST
      case (-1, 1) => SOUTHWEST
      case _ => INVALID
    }
  }
}
