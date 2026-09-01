package cardgamewar

import Suit.*
import FaceCard.*
import AceCard.*
import DiscardPile.*
import Player.*
import Outcome.*
import cats.Order
import cats.syntax.all.*
import cats.data.State

enum Player:
  case PlayerOne, PlayerTwo

enum Suit:
  case Spade, Club, Diamond, Heart

enum FaceCard:
  case Jack, Queen, King

enum AceCard:
  case Ace

enum DiscardPile:
  case Discard

enum Outcome:
  case PlayerOneWins, PlayerTwoWins, Draw

type Rank = 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | FaceCard | AceCard

case class Card(rank: Rank, suit: Suit)

type Cards = Seq[Card]

type CardDesignee = Player | DiscardPile

type CardHands = Map[CardDesignee, Cards]

case class CardGameWar(cardHands: CardHands, outcome: Option[Outcome] = None)

sealed trait RoundWinner {
  def player: Player
  def card: Card
}

case class BattleWinner(player: Player, card: Card) extends RoundWinner

case class WarWinner(player: Player, card: Card) extends RoundWinner

type GameState[A] = State[CardGameWar, A]

val ranks: Seq[Rank] = Seq(2, 3, 4, 5, 6, 7, 8, 9, 10, Jack, Queen, King, Ace)

val suits: Seq[Suit] = Seq(Spade, Club, Diamond, Heart)

extension (rank: Rank)

  def rankValue: Int = ranks.indexOf(rank)

extension (player: Player)

  def other: Player = player match {
    case PlayerOne => PlayerTwo
    case PlayerTwo => PlayerOne
  }

  def wins: Outcome = player match {
    case PlayerOne => PlayerOneWins
    case PlayerTwo => PlayerTwoWins
  }

given Order[Card] {
  override def compare(x: Card, y: Card): Int = x.rank.rankValue.compare(y.rank.rankValue)
}

val cards: Seq[Card] = for {
  rank <- ranks
  suit <- suits
} yield Card(rank = rank, suit = suit)

// object GameState {
//   def apply(cardsA: Cards, cardsB: Cards): GameState =
//     (Map(PlayerOne -> cardsA, PlayerTwo -> cardsB), None)
// }
