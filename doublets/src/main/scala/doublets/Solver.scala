package doublets

import cats.syntax.all.*
import scala.collection.immutable.Queue
import scala.annotation.tailrec
import io.circe.parser.parse
import scala.io.Source

object Solver {

  val words: Seq[String] =
    parse(Source.fromResource("words.json").mkString)
      .flatMap(_.as[Seq[String]])
      .fold(
        e => throw Exception(s"Problem parsing words.json: $e"),
        identity
      )

  def variesByOne(word1: String, word2: String): Boolean =
    word1.size == word2.size &&
      (word1.toSeq.zip(word2.toSeq).count { (a, b) =>
        a != b
      } == 1)

  def doublets(word1: String, word2: String): Seq[String] =
    @tailrec
    def loop(q: Queue[Seq[String]], seen: Set[String]): Seq[String] =
      if q.isEmpty then Seq.empty
      else
        val (wordLinksChain, q_) = q.dequeue
        wordLinksChain.lastOption match {
          case None                                => ???
          case Some(lastWord) if lastWord == word2 => wordLinksChain
          case Some(lastWord)                      => {
            val wordsToLink = words.filter(word => !seen(word) && variesByOne(lastWord, word))
            loop(q_.appendedAll(wordsToLink.map(wordLinksChain.appended(_))), seen.++(wordsToLink))
          }
        }
    loop(Queue(Seq(word1)), Set(word1))
}
