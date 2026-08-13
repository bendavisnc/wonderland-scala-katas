package tinymaze

sealed trait IMazeStep {
  def position: MazePosition
  def maze: Maze
  def mazeSize: MazeSize
}

final case class MazeStep(
  override val position: MazePosition,
  override val maze: Maze,
  override val mazeSize: MazeSize
) extends IMazeStep

final case class MazeFirstStep(
  override val position: MazePosition,
  override val maze: Maze,
  override val mazeSize: MazeSize
) extends IMazeStep

final case class MazeLastStep(
  override val position: MazePosition,
  override val maze: Maze,
  override val mazeSize: MazeSize
) extends IMazeStep

object MazeFirstStep {
  def apply(maze: Maze): MazeFirstStep =
    MazeFirstStep(position = (0, 0), maze = maze, mazeSize = (maze.head.size, maze.size))
}

object MazeLastStep {
  def apply(mazeStep: IMazeStep): MazeLastStep =
    MazeLastStep(position = mazeStep.position, maze = mazeStep.maze, mazeSize = mazeStep.mazeSize)
}
