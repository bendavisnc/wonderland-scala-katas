package cardgamewar

import cardgamewar.Game.*
import cardgamewar.Player.*
import cardgamewar.DiscardPile.*
import cardgamewar.FaceCard.*
import cardgamewar.AceCard.*
import cardgamewar.Suit.*
import cats.data.State
import cats.data.StateT

class GameTomAndMaryTest extends munit.FunSuite {

  test("play game, turn one") {
    val tomsCards            = Seq(Card(4, Club))
    val marysCards           = Seq(Card(8, Spade))
    val cardHands: CardHands =
      Map(PlayerOne -> tomsCards, PlayerTwo -> marysCards, Discard -> Seq.empty)
    val ((cardHandsUpdated, _), Some(winningCard)) = playRound((cardHands, None)): @unchecked
    assertEquals(
      winningCard,
      marysCards.head
    )
    assertEquals(cardHandsUpdated(PlayerOne).size, 0)
    assertEquals(cardHandsUpdated(PlayerTwo).size, 2)
    assertEquals(cardHandsUpdated(Discard).size, 0)
  }

  test("play game, turn two") {
    val tomsCards  = Seq(Card(Ace, Spade))
    val marysCards = Seq(Card(King, Spade))

    val cardHands: CardHands =
      Map(PlayerOne -> tomsCards, PlayerTwo -> marysCards, Discard -> Seq.empty)
    val ((cardHandsUpdated, _), Some(winningCard)) = playRound((cardHands, None)): @unchecked
    assertEquals(
      winningCard,
      tomsCards.head
    )
    assertEquals(cardHandsUpdated(PlayerOne).size, 2)
    assertEquals(cardHandsUpdated(PlayerTwo).size, 0)
    assertEquals(cardHandsUpdated(Discard).size, 0)
  }

  test("play game, turn three") {
    val tomsCards =
      Seq(Card(6, Spade), Card(2, Spade), Card(3, Spade), Card(4, Spade), Card(8, Diamond))
    val marysCards =
      Seq(Card(6, Diamond), Card(2, Diamond), Card(3, Diamond), Card(4, Diamond), Card(9, Heart))

    val cardHands: CardHands =
      Map(PlayerOne -> tomsCards, PlayerTwo -> marysCards, Discard -> Seq.empty)
    val ((cardHandsUpdated, _), Some(winningCard)) = playRound(cardHands, None): @unchecked
    assertEquals(
      winningCard,
      marysCards.last
    )
    assertEquals(cardHandsUpdated(PlayerOne).size, 0)
    assertEquals(cardHandsUpdated(PlayerTwo).size, 10)
    assertEquals(cardHandsUpdated(Discard).size, 0)
  }

}
