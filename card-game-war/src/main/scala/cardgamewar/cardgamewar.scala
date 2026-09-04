package cardgamewar

import Suit.*
import FaceCard.*
import AceCard.*
import DiscardPile.*
import Player.*
import cats.Order
import cats.syntax.all.*
import cats.data.State

val WarCardsCount = 4

val ranks: Seq[Rank] = Seq(2, 3, 4, 5, 6, 7, 8, 9, 10, Jack, Queen, King, Ace)

val suits: Seq[Suit] = Seq(Spade, Club, Diamond, Heart)

val cards: Cards = for {
  rank <- ranks
  suit <- suits
} yield Card(rank = rank, suit = suit)

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

enum BattleResult:
  case BattleWinner(player: Player, card: Card) extends BattleResult with RoundWinner
  case War

enum WarResult:
  case WarWinner(player: Player, card: Card) extends WarResult with RoundWinner
  case Draw                                  extends WarResult with RoundResult

sealed trait RoundResult

type Rank = 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | FaceCard | AceCard

case class Card(rank: Rank, suit: Suit)

type Cards = Seq[Card]

type CardDesignee = Player | DiscardPile

type CardHands = Map[CardDesignee, Cards]

sealed trait RoundWinner extends RoundResult {
  def player: Player
  def card: Card
}

type GameState[A] = State[CardHands, A]

extension (rank: Rank)

  def rankValue: Int = ranks.indexOf(rank)

extension (player: Player)

  def other: Player = player match {
    case PlayerOne => PlayerTwo
    case PlayerTwo => PlayerOne
  }

extension (cardHands: CardHands)

  def drawCard(cardDesignee: CardDesignee): (CardHands, Option[Card]) =
    cardHands.get(cardDesignee) match
      case Some(card :: cards) => (cardHands.updated(cardDesignee, cards), Some(card))
      case _                   => (cardHands, None)

  def drawDiscardPile: (CardHands, Seq[Card]) =
    (cardHands.-(Discard), cardHands.get(Discard).toSeq.flatten)

  def addCard(cardDesignee: CardDesignee, card: Card): CardHands =
    cardHands.updatedWith(cardDesignee)(_.map(_.appended(card)).orElse(Some(Seq(card))))

  def isGameOver: Boolean =
    cardHands.-(Discard).values.map(_.size).toSeq.contains(0)

given Order[Card] {
  override def compare(x: Card, y: Card): Int = x.rank.rankValue.compare(y.rank.rankValue)
}
