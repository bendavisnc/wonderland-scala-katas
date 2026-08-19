package cardgamewar

import cardgamewar.FaceCard.*
import cardgamewar.AceCard.*
import cardgamewar.Suit.*
import cardgamewar.Player.*
import cats.syntax.all.*
import scala.annotation.tailrec

object Game {

  def gameWinner(playerOneCards: Seq[Card], playerTwoCards: Seq[Card]): Option[EndGame] =
    (playerOneCards, playerTwoCards) match {
      case (Nil, Nil) =>
        throw new Exception("Unexpected game state, both players' hands are empty.")

      case (_, Nil) =>
        Some(
          EndGame(winner = PlayerOne,
                  playerOneCards = playerOneCards,
                  playerTwoCards = playerTwoCards
          )
        )
      case (Nil, _) =>
        Some(
          EndGame(winner = PlayerTwo,
                  playerOneCards = playerOneCards,
                  playerTwoCards = playerTwoCards
          )
        )
      case _ => None
    }

  def playRound(playerOneCard: Card, playerTwoCard: Card): Option[Card] =
    (playerOneCard.rank.rankValue > playerTwoCard.rank.rankValue,
     playerTwoCard.rank.rankValue > playerOneCard.rank.rankValue
    ) match {
      case (true, _) => Some(playerOneCard)
      case (_, true) => Some(playerTwoCard)
      case _         => None
    }

  def roundWinner(playerOneCards: Seq[Card], playerTwoCards: Seq[Card]): Option[EndRound] =
    val playerOneTopCardOpt     = playerOneCards.headOption
    val playerOneRemainingCards = playerOneCards.tail
    val playerTwoTopCardOpt     = playerTwoCards.headOption
    val playerTwoRemainingCards = playerTwoCards.tail

    playerOneTopCardOpt
      .orElse(throw new Exception("Unexpected game state, player one ran out of cards."))
      .flatMap(playerOneTopCard => {
        playerTwoTopCardOpt
          .orElse(throw new Exception("Unexpected game state, player two ran out of cards."))
          .flatMap(playerTwoTopCard => {
            playRound(playerOneTopCard, playerTwoTopCard).map {
              case winningCard if winningCard == playerOneTopCard =>
                EndRound(
                  PlayerOne,
                  playerOneCards =
                    playerOneRemainingCards.appended(playerOneTopCard).appended(playerTwoTopCard),
                  playerTwoCards = playerTwoRemainingCards
                )
              case winningCard if winningCard == playerTwoTopCard =>
                EndRound(
                  PlayerTwo,
                  playerOneCards = playerOneRemainingCards,
                  playerTwoCards =
                    playerTwoRemainingCards.appended(playerTwoTopCard).appended(playerOneTopCard)
                )
              case _ => ???
            }
          })
      })

  def roundWinnerTieBreak(playerOneCards: Seq[Card],
                          playerTwoCards: Seq[Card],
                          limboCards: Seq[Card] = Seq.empty
  ): EndRound =
    Option
      .when(playerOneCards.size <= 4 || playerTwoCards.size <= 4)(())
      .flatMap(_ => {
        Option
          .when(playerTwoCards.size < playerOneCards.size)(())
          .map(_ => {
            EndRound(winner = PlayerOne,
                     playerOneCards = playerOneCards.concat(playerTwoCards).concat(limboCards),
                     playerTwoCards = Seq.empty
            )
          })
          .orElse(
            Option
              .when(playerOneCards.size < playerTwoCards.size)(())
              .map(_ => {
                EndRound(winner = PlayerTwo,
                         playerOneCards = Seq.empty,
                         playerTwoCards = playerTwoCards.concat(playerOneCards).concat(limboCards)
                )
              })
          )
          .orElse(
            Some(
              EndRound(winner = PlayerOne,
                       playerOneCards = playerOneCards.concat(playerTwoCards).concat(limboCards),
                       playerTwoCards = Seq.empty
              )
            )
          )
      })
      .getOrElse {
        val playerOneTopCards       = playerOneCards.take(4)
        val playerOneCardsRemaining = playerOneCards.drop(4)
        val playerTwoTopCards       = playerTwoCards.take(4)
        val playerTwoCardsRemaining = playerTwoCards.drop(4)
        val playerOneLastTopCard    = playerOneTopCards.last
        val playerTwoLastTopCard    = playerTwoTopCards.last
        playRound(playerOneLastTopCard, playerTwoLastTopCard) match {
          case Some(winningCard) if winningCard == playerOneLastTopCard =>
            EndRound(
              winner = PlayerOne,
              playerOneCards = playerOneCardsRemaining
                .concat(playerOneTopCards)
                .concat(playerTwoTopCards)
                .concat(limboCards),
              playerTwoCards = playerTwoCardsRemaining
            )
          case Some(winningCard) if winningCard == playerTwoLastTopCard =>
            EndRound(
              winner = PlayerTwo,
              playerOneCards = playerOneCardsRemaining,
              playerTwoCards = playerTwoCardsRemaining
                .concat(playerTwoTopCards)
                .concat(playerOneTopCards)
                .concat(limboCards)
            )
          case _ =>
            roundWinnerTieBreak(
              playerOneCards = playerOneCardsRemaining,
              playerTwoCards = playerTwoCardsRemaining,
              limboCards = limboCards.concat(playerOneTopCards).concat(playerTwoTopCards)
            )
        }
      }

  def playGame(playerOneCards: Seq[Card], playerTwoCards: Seq[Card]): EndGame =
    val gameWinnerOpt = gameWinner(playerOneCards, playerTwoCards)
    gameWinnerOpt.getOrElse(
      roundWinner(playerOneCards, playerTwoCards)
        .getOrElse(default = roundWinnerTieBreak(playerOneCards, playerTwoCards)) match {
        case EndRound(_, cardsA, cardsB) =>
          playGame(cardsA, cardsB)
      }
    )

}
