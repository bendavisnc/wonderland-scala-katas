package doublets

import cats.syntax.all.*
import scala.collection.immutable.Queue
import scala.annotation.tailrec
import io.circe.parser.parse
import scala.io.Source
import cats.data.NonEmptySeq

object Solver {

  type Word      = String
  type WordChain = NonEmptySeq[Word]

  val words: Seq[Word] =
    parse(Source.fromResource("words.json").mkString)
      .flatMap(_.as[Seq[Word]])
      .fold(
        e => throw Exception(s"Problem parsing words.json: $e"),
        identity
      )

  def variesByOne(word1: Word, word2: Word): Boolean =
    word1.size == word2.size &&
      (word1.toSeq.zip(word2.toSeq).count { (a, b) =>
        a != b
      } == 1)

  def doublets(word1: Word, word2: Word): Seq[Word] =
    @tailrec
    def loop(q: Queue[WordChain], seen: Set[String]): Seq[String] =
      if q.isEmpty then Seq.empty
      else
        val (wordChain, q_) = q.dequeue
        val lastWord        = wordChain.last
        if lastWord == word2 then wordChain.toSeq
        else {
          val wordsToLink = words.filter(word => !seen(word) && variesByOne(lastWord, word))
          loop(q_.appendedAll(wordsToLink.map(wordChain.append(_))), seen.++(wordsToLink))
        }
    loop(Queue(NonEmptySeq.one(word1)), Set(word1))
}
