package snakey

import cats.effect.IO
import cats.effect.std.Random

case class Snake(direction: Direction, head: Coord, tail: List[Coord]) {
  def changeDirection(input: Input): Snake = Snake(direction.change(input), head, tail)
  def grow(at: Coord): Snake               = Snake(direction, at, head :: tail)

  def advance(boardDim: Dim): Snake = {
    val newHead = direction match {
      case Direction.left  => head.left(boardDim)
      case Direction.right => head.right(boardDim)
      case Direction.up    => head.up(boardDim)
      case Direction.down  => head.down(boardDim)
    }
    Snake(direction, newHead, (head :: tail).dropRight(1))
  }
}

case class Rng(newPellet: Coord)
object Rng {
  def generate(game: Game)(implicit random: Random[IO]): IO[Rng] = {
    val avoid = (game.snake.head :: game.snake.tail).toSet + game.pellets
    random.elementOf(game.zeroBoard.pixels.keys.filterNot(avoid)).map(Rng.apply).orElse(IO.pure(empty))
  }

  def empty: Rng = Rng(Coord(0, 0))
}

case class Game(
  zeroBoard: Board, // this won't be modified, we use it as an empty board to which we render each frame
  nextInput: InputBuffer,
  snake: Snake,
  controls: Controls,
  pellets: Set[Coord],
  time: Int,
  rng: Rng,
  animation: Option[AnimationRunner[Int]],
) {

  def tick(rng: Rng): (Game, Board) = {
    val next = copy(rng = rng).runAnimation.processInput.checkCollision.hitPellets.generatePellets.advanceTime
    (next, next.render)
  }

  def acceptInput(key: Int): Game = controls.parse(key).map(i => copy(nextInput = nextInput.put(i))).getOrElse(this)

  private def render: Board = {
    val pixels = animation match {
      case Some(anim) => anim.pixels
      case None =>
        val headColor   = Color.red
        val tailColor   = Color.green
        val pelletColor = Color.yellow
        pellets.toVector.map(_ -> pelletColor) ++ snake.tail.map(_ -> tailColor).appended(snake.head -> headColor)
    }

    pixels.foldLeft(zeroBoard) { case (board, (coord, color)) => board.set(coord, color) }
  }

  private def processInput: Game = {
    val (input, unprocessed) = nextInput.uncons match { // input is buffered, process one input per frame
      case Some((input, unprocessed)) => Some(input) -> unprocessed
      case None                       => None        -> Vector.empty[Input]
    }
    copy(
      nextInput = InputBuffer(unprocessed),
      snake = input.map(snake.changeDirection).getOrElse(snake).advance(zeroBoard.dim)
    )
  }

  private def hitPellets: Game = {
    val predictHead = snake.advance(zeroBoard.dim).head
    val willHit     = pellets.contains(predictHead)
    copy(
      snake = if (willHit) snake.grow(predictHead) else snake,
      pellets = if (willHit) pellets - predictHead else pellets
    )
  }

  private def advanceTime: Game = copy(time = time + 1)
  private def generatePellets: Game = {
    def generate: Set[Coord] = pellets + rng.newPellet
    copy(pellets = if (time % 35 == 0) generate else pellets)
  }

  private def checkCollision: Game = {
    val predict     = snake.advance(zeroBoard.dim)
    val predictHead = predict.head
    val predictTail = predict.tail

    if (predictTail.contains(predictHead) && animation.isEmpty)
      // game over
      copy(animation = Some(AnimationRunner.start(zeroBoard.dim, Animation.verticalScan)))
    else
      this
  }

  private def runAnimation: Game = copy(animation = animation.map(_.tick))
}

object Game {
  def build(keyboard: Keyboard, controls: Controls, rng: Rng): Game = Game(
    zeroBoard = Board.build(width = keyboard.width, height = keyboard.height),
    nextInput = InputBuffer.empty,
    snake = Snake(Direction.right, Coord.zero, Nil),
    controls = controls,
    pellets = Set(),
    time = 0,
    rng = rng,
    animation = None
  )
}
