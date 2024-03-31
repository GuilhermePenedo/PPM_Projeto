import Main.Coord2D

object Direction extends Enumeration {
  type Direction = Value
  val North, South, East, West,
  NorthEast, NorthWest, SouthEast, SouthWest = Value


  def nextPos(direction: Direction, pos: Coord2D):Coord2D = direction match{
    case North => (pos._1, pos._2-1)
    case South => (pos._1, pos._2+1)
    case East => (pos._1 -1, pos._2)
    case West => (pos._1 +1, pos._2)
    case NorthEast => (pos._1 -1, pos._2-1)
    case NorthWest => (pos._1 +1, pos._2-1)
    case SouthEast => (pos._1 -1, pos._2+1)
    case SouthWest => (pos._1 +1, pos._2+1)
  }
}
