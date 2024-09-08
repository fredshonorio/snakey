package snakey

trait Animation[STATE] {
  def init: STATE
  def next(board: Dim, current: STATE): (Vector[(Coord, Color)], STATE)

}

object Animation {

  // looks kinda weird because we can't push this pixels at this rate
  val verticalScan: Animation[Int] = new Animation[Int] {
    override def init: Int = 0 // column
    override def next(board: Dim, current: Int): (Vector[(Coord, Color)], Int) = {
      val x = board.wrap(Coord(current + 1, 0)).x
      (
        Vector.range(0, board.height).map(y => Coord(x, y) -> Color.red),
        current + 1
      )
    }
  }
}

case class AnimationRunner[A](board: Dim, anim: Animation[A], state: A, pixels: Vector[(Coord, Color)]) {
  def tick: AnimationRunner[A] = {
    val (pixels, nextState) = anim.next(board, state)
    AnimationRunner(board, anim, nextState, pixels)
  }
}

object AnimationRunner {
  def start[A](board: Dim, animation: Animation[A]): AnimationRunner[A] =
    AnimationRunner(board, animation, animation.init, Vector.empty)
}
