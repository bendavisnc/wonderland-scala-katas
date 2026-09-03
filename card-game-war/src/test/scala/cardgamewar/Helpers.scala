package cardgamewar

import cardgamewar.CardHands
import cardgamewar.cards
import cardgamewar.Player.*

import org.scalacheck.Gen
import org.scalacheck.Arbitrary

object Helpers {

  def splitGen[A](as: Seq[A]): Gen[(Seq[A], Seq[A])] =
    for
      shuffled <- Gen.pick(as.size, as).map(_.toSeq)
      splitAt  <- Gen.choose(0, as.size)
    yield shuffled.splitAt(splitAt)

  val g: Gen[CardHands] = splitGen(cards).map { case (cardsA, cardsB) =>
    Map(PlayerOne -> cardsA, PlayerTwo -> cardsB)
  }

  given h: Arbitrary[CardHands] = Arbitrary(g)
}
