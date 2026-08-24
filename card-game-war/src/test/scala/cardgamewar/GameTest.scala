package cardgamewar

import cardgamewar.*
import cardgamewar.Game.*
import cardgamewar.Player.*
import cardgamewar.Outcome.*
import cardgamewar.DiscardPile.*
import cardgamewar.FaceCard.*
import cardgamewar.AceCard.*
import cardgamewar.Suit.*
import cats.data.State
import cats.data.StateT

class GameTest extends munit.FunSuite {

  test("play round, the highest rank wins the cards in the round") {
    val (_, Some(winningCard)) =
      playRound(GameState(Seq(cards.head), Seq(cards.last))): @unchecked
    assertEquals(winningCard, cards.last)
  }

  test("play round, queens are higher rank than jacks") {
    val (_, Some(winningCard)) =
      playRound(GameState(Seq(Card(Queen, Heart)), Seq(Card(Jack, Heart)))): @unchecked
    assertEquals(winningCard, Card(Queen, Heart))
  }

  test("play round, kings are higher rank than queens") {
    val (_, Some(winningCard)) =
      playRound(GameState(Seq(Card(King, Heart)), Seq(Card(Queen, Heart)))): @unchecked
    assertEquals(winningCard, Card(King, Heart))

  }

  test("play round, aces are higher rank than kings") {
    val (_, Some(winningCard)) =
      playRound(GameState(Seq(Card(Ace, Heart)), Seq(Card(King, Heart)))): @unchecked
    assertEquals(winningCard, Card(Ace, Heart))
  }

  test("play game, the player loses when they run out of cards, unless hands dealt are mirrored") {
    val (playerOneCards, playerTwoCards) =
      cards.zipWithIndex.foldLeft((Seq.empty[Card], Seq.empty[Card])) {
        case ((cardsA, cardsB), (card, i)) if i % 2 == 1 => (cardsA.appended(card), cardsB)
        case ((cardsA, cardsB), (card, i)) => (cardsA, cardsB.appended(card))
      }
    assertEquals((playerOneCards.size, playerTwoCards.size), (26, 26))
    val ((cardHands, _), outcome) = playGame(GameState(playerOneCards, playerTwoCards))
    assertEquals(cardHands.get(PlayerOne).toSeq.flatten.size, 2)
    assertEquals(cardHands.get(PlayerTwo).toSeq.flatten.size, 2)
    assertEquals(cardHands.get(Discard).toSeq.flatten.size, 48)
    assertEquals(Draw, outcome)
  }

  test("play game, the player loses when they run out of cards") {
    val (cardsA, cardsB) =
      cards.zipWithIndex.foldLeft((Seq.empty[Card], Seq.empty[Card])) {
        case ((cardsA, cardsB), (card, i)) if i % 2 == 1 => (cardsA.appended(card), cardsB)
        case ((cardsA, cardsB), (card, i)) => (cardsA, cardsB.appended(card))
      }
    val playerOneCards = Seq(cardsB.last).concat(cardsA)
    val playerTwoCards = cardsB.dropRight(1)
    assertEquals((playerOneCards.size, playerTwoCards.size), (27, 25))
    val ((cardHands, _), outcome) = playGame(GameState(playerOneCards, playerTwoCards))
    assertEquals(cardHands.get(PlayerOne).toSeq.flatten.size, 4)
    assertEquals(cardHands.get(PlayerTwo).toSeq.flatten.size, 0)
    assertEquals(cardHands.get(Discard).toSeq.flatten.size, 48)
    assertEquals(PlayerOneWins, outcome)
  }

}
