package tinymaze

sealed trait IMazeStep(
  position: MazePosition,
  val maze: Maze,
  mazeSize: MazeSize
)

final case class MazeStep(
  position: MazePosition,
  override val maze: Maze,
  mazeSize: MazeSize
) extends IMazeStep(position, maze, mazeSize)

final case class MazeFirstStep(
  position: MazePosition,
  override val maze: Maze,
  mazeSize: MazeSize
) extends IMazeStep(position, maze, mazeSize)

final case class MazeLastStep(
  position: MazePosition,
  override val maze: Maze,
  mazeSize: MazeSize
) extends IMazeStep(position, maze, mazeSize)

object MazeFirstStep {
  def apply(maze: Maze): MazeFirstStep =
    MazeFirstStep(position = (0, 0), maze = maze, mazeSize = (maze.head.size, maze.size))
}
