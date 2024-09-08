package snakey

case class Dim(width: Int, height: Int) {
  def wrap(coord: Coord): Coord = Coord((coord.x + width) % width, (coord.y + height) % height)
}

case class Board(dim: Dim, pixels: Map[Coord, Color], empty: Map[Coord, Color]) {
  def diff(next: Board): Vector[(Coord, Color)] = {
    val changed = next.pixels.view.filter { case (k, color) =>
      val prev = pixels.get(k)
      prev.isEmpty || prev.exists(_ != color)
    }.toVector

    val erased = pixels.keySet.removedAll(next.pixels.keySet).map(k => k -> Color.black)

    changed ++ erased
  }

  def set(coord: Coord, color: Color): Board = Board(dim, pixels + (coord -> color), empty)
}

object Board {
  def build(width: Int, height: Int): Board = {
    val empty = List.range(0, width).flatMap(x => List.range(0, height).map(y => Coord(x, y) -> Color.black)).toMap
    Board(Dim(width, height), empty, empty)
  }
}

case class Coord(x: Int, y: Int) {
  def left(boardDim: Dim): Coord  = boardDim.wrap(Coord(x - 1, y))
  def right(boardDim: Dim): Coord = boardDim.wrap(Coord(x + 1, y))
  def up(boardDim: Dim): Coord    = boardDim.wrap(Coord(x, y - 1))
  def down(boardDim: Dim): Coord  = boardDim.wrap(Coord(x, y + 1))

}

object Coord {
  def zero: Coord = Coord(0, 0)
}
