package cardgamewar

import cats.syntax.all.*
import scala.collection.immutable.Queue
import scala.annotation.tailrec

object Game {

  enum Player:
    case PlayerOne, PlayerTwo

  enum Suit:
    case Spade, Club, Diamond, Heart

  enum Face:
    case Jack, Queen, King

  enum Ace:
    case Ace

  type Rank = 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | Face | Ace

  type Card = (Suit, Rank)

  def playGame(playerOneCards: Seq[Card], playerTwoCards: Seq[Card]): Player = ???

}
