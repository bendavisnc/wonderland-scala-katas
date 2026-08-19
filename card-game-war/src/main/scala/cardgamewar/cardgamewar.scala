package cardgamewar

import Suit.*
import FaceCard.*
import AceCard.*

enum Player:
  case PlayerOne, PlayerTwo

enum Suit:
  case Spade, Club, Diamond, Heart

enum FaceCard:
  case Jack, Queen, King

enum AceCard:
  case Ace

type Rank = 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | FaceCard | AceCard

case class Card(rank: Rank, suit: Suit)

case class EndGame(winner: Player, playerOneCards: Seq[Card], playerTwoCards: Seq[Card])

case class EndRound(winner: Player, playerOneCards: Seq[Card], playerTwoCards: Seq[Card])

val ranks: Seq[Rank] = Seq(2, 3, 4, 5, 6, 7, 8, 9, 10, Jack, Queen, King, Ace)

val suits: Seq[Suit] = Seq(Spade, Club, Diamond, Heart)

extension (rank: Rank)

  def rankValue: Int = ranks.indexOf(rank)

val cards: Seq[Card] = for {
  rank <- ranks
  suit <- suits
} yield Card(rank = rank, suit = suit)
