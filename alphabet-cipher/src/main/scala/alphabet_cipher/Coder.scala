package alphabet_cipher

import cats.syntax.all.*

object Coder {

  def alphabetCipherChartNextRow(row: Seq[Char]): Seq[Char] =
    row.toList match {
      case firstChar :: secondToLastChars => secondToLastChars.appended(firstChar)
      case _                              => ???
    }

  def charToIndex(c: Char): Int = c - 'a'

  val alphabetCipherChart: Map[Char, Map[Char, Char]] =
    val alphabetChars: Seq[Char] = 'a' to 'z'
    assert(alphabetChars.size == 26)
    val characterGrid: Seq[Seq[Char]] =
      LazyList
        .iterate(start = alphabetChars)(alphabetCipherChartNextRow)
        .take(alphabetChars.size)

    alphabetChars
      .zip(characterGrid)
      .map { case (columnChar, row) =>
        columnChar -> alphabetChars.zip(row).toMap
      }
      .toMap

  def repeatingSubstring(s: String): String =
    Range(1, s.size)
      .map(i => s.substring(0, i))
      .find { subs =>
        LazyList.continually(subs).flatten.take(s.size).mkString == s
      }
      .getOrElse(throw Exception(s"Couldn't find repeating string for `$s`."))

  def encode(keyword: String, message: String): String =
    LazyList
      .continually(keyword.toCharArray().toSeq)
      .flatten
      .zip(message.toCharArray().toSeq)
      .map { case (columnChar, rowChar) => alphabetCipherChart(columnChar)(rowChar) }
      .mkString

  def decode(keyword: String, message: String): String =
    LazyList
      .continually(keyword.toCharArray().toSeq)
      .flatten
      .zip(message.toCharArray().toSeq)
      .map { case (columnChar, rowChar) => alphabetCipherChart(columnChar).map(_.swap)(rowChar) }
      .mkString

  def decipher(cipher: String, message: String): String =
    repeatingSubstring(decode(message, cipher))

}
