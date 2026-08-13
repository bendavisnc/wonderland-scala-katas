package tinymaze

type MazePosition = (Int, Int)

type Direction = (Int, Int)

type MazeSize = (Int, Int)

type MazeElement = 0 | 1 | 's' | 'e' | 'x'

type Maze = Seq[Seq[MazeElement]]

type MazeSteps = Seq[IMazeStep]

val O: MazeElement = 0
val I: MazeElement = 1
val S: MazeElement = 's'
val E: MazeElement = 'e'
val X: MazeElement = 'x'

val Left: Direction  = (-1, 0)
val Right: Direction = (1, 0)
val Up: Direction    = (0, -1)
val Down: Direction  = (0, 1)

extension (maze: Maze)
  def set(position: MazePosition, element: MazeElement): Maze =
    maze.patch(position._2, Seq(maze(position._2).patch(position._1, Seq(element), 1)), 1)

  def get(position: MazePosition): Option[MazeElement] =
    for {
      row     <- maze.lift(position._2)
      element <- row.lift(position._1)
    } yield element

extension (position: MazePosition)
  def within(size: MazeSize): Boolean =
    (position, size) match {
      case ((x, y), (x2, y2)) => x < x2 && y < y2
    }
