package magicsquare

import cats.syntax.all.*

object Puzzle {

  val values = Seq(1.0, 1.5, 2.0, 2.5, 3.0, 3.5, 4.0, 4.5, 5.0)

  type Square = Seq[Seq[Double]]

  def square(acc: Seq[Double]): Square =
    acc.grouped(3).toSeq

  def rowSums(m: Square): Seq[Double] = m.map(_.combineAll)

  def columnSums(m: Square): Seq[Double] =
    Range(0, m.head.size).map(rowIndex => {
      (Range(0, m.size)
        .map(columnIndex => {
          m(columnIndex)(rowIndex)
        }): Seq[Double]).combineAll
    })

  def diagonalSums(m: Square): Seq[Double] =
    Range(0, m.size)
      .zip(Range(m.size - 1, -1, -1))
      .map { case (i, j) =>
        (m(i)(i), m(j)(i))
      }
      .unzip
      .toList
      .map((_: Seq[Double]).combineAll)

  def isMagical(m: Square): Boolean =
    (rowSums.combine(columnSums).combine(diagonalSums): Square => Seq[Double])
      .map(_.toSet)
      .andThen(_.size == 1)
      .apply(m)

  def magicSquare(values: Seq[Double]): Square =
    values.permutations
      .map(square)
      .find(isMagical)
      .getOrElse(throw new Exception("Failed to find magic square."))
}
