package cardgamewar

import cardgamewar.FaceCard.*
import cardgamewar.AceCard.*
import cardgamewar.Suit.*
import cardgamewar.Player.*
import cardgamewar.DiscardPile.*
import cats.syntax.all.*
import scala.annotation.tailrec
import cats.data.State
import cats.Applicative
import cats.Traverse
import pprint.pprintln

object Game {

  def drawCard(player: Player): GameState[Option[Card]] =
    State
      .inspect((_.get(player).flatMap(_.headOption)): CardHands => Option[Card])
      .flatMap {
        case topCard @ Some(_) =>
          State
            .modify[CardHands](cardHands => cardHands.updated(player, cardHands.apply(player).tail))
            .map(_ => topCard)
        case None => State.get.map(_ => None)
      }

  def drawDiscardPile: GameState[Seq[Card]] =
    State
      .inspect((_.get(Discard).toSeq.flatten): CardHands => Seq[Card])
      .flatMap { case discardCards =>
        Traverse[Seq]
          .traverse(discardCards)((card: Card) => {
            State.modify[CardHands](
              (cardHands => cardHands.-(Discard)): CardHands => CardHands
            )
          })
          .map(_ => discardCards)
      }

  def drawCards: GameState[(Option[Card], Option[Card])] =
    (drawCard(PlayerOne), drawCard(PlayerTwo)).tupled

  def addCard(cardDesignee: CardDesignee, card: Card, cardHands: CardHands): CardHands =
    cardHands.updated(cardDesignee, cardHands.get(cardDesignee).toSeq.flatten.appended(card))

  def drawCardsForWar: GameState[(Option[List[Card]], Option[List[Card]])] =
    def f(player: Player) = Traverse[Seq]
      .traverse(Seq.fill(WarCardsCount)(player))(drawCard)
      .map(_.sequence)
      .map(_.map(_.toList))
    (f(PlayerOne), f(PlayerTwo)).tupled

  def playBattle(
    gamestate: GameState[(Option[Card], Option[Card])]
  ): GameState[Option[BattleWinner]] =
    gamestate.flatMap {
      case (Some(cardA), Some(cardB)) if cardA > cardB =>
        drawDiscardPile.flatMap { case cardsDiscarded =>
          Traverse[Seq]
            .traverse(cardA :: cardB :: cardsDiscarded.toList)((card: Card) => {
              State.modify[CardHands](
                (addCard(PlayerOne, card, _: CardHands)): CardHands => CardHands
              )
            })
            .map(_ => BattleWinner(player = PlayerOne, card = cardA).some)
        }
      case (Some(cardA), Some(cardB)) if cardA < cardB =>
        drawDiscardPile.flatMap { case cardsDiscarded =>
          Traverse[Seq]
            .traverse(cardB :: cardA :: cardsDiscarded.toList)((card: Card) => {
              State.modify[CardHands](
                (addCard(PlayerTwo, card, _: CardHands)): CardHands => CardHands
              )
            })
            .map(_ => BattleWinner(player = PlayerTwo, card = cardB).some)
        }
      case (Some(cardA), Some(cardB)) =>
        State
          .modify[CardHands](
            ((addCard(Discard, cardA, _: CardHands)): CardHands => CardHands)
              .andThen(addCard(Discard, cardB, _: CardHands))
          )
          .map(_ => None)

      case _ => throw Exception("One or both players are missing a card.")

    }

  def playWar(
    gamestate: GameState[(Option[List[Card]], Option[List[Card]])]
  ): GameState[Option[WarWinner]] =
    gamestate.flatMap {
      case (Some(playerOneCards @ _ :: _ :: _ :: cardA :: Nil),
            Some(playerTwoCards @ _ :: _ :: _ :: cardB :: Nil)
          ) if cardA > cardB => {
        drawDiscardPile.flatMap { case cardsDiscarded =>
          Traverse[Seq]
            .traverse(playerOneCards.concat(playerTwoCards).concat(cardsDiscarded))(
              (card: Card) => {
                State.modify[CardHands](
                  (addCard(PlayerOne, card, _: CardHands)): CardHands => CardHands
                )
              }
            )
            .map(_ => Some(WarWinner(player = PlayerOne, card = cardA)))
        }
      }

      case (Some(playerOneCards @ _ :: _ :: _ :: cardA :: Nil),
            Some(playerTwoCards @ _ :: _ :: _ :: cardB :: Nil)
          ) if cardA < cardB => {
        drawDiscardPile.flatMap { case cardsDiscarded =>
          Traverse[Seq]
            .traverse(playerOneCards.concat(playerTwoCards).concat(cardsDiscarded))(
              (card: Card) => {
                State.modify[CardHands](
                  (addCard(PlayerTwo, card, _: CardHands)): CardHands => CardHands
                )
              }
            )
            .map(_ => Some(WarWinner(player = PlayerTwo, card = cardB)))
        }
      }

      case (Some(playerOneCards @ _ :: _ :: _ :: _ :: Nil),
            Some(playerTwoCards @ _ :: _ :: _ :: _ :: Nil)
          ) => {
        drawDiscardPile.flatMap { case cardsDiscarded =>
          Traverse[Seq]
            .traverse(playerOneCards.concat(playerTwoCards).concat(cardsDiscarded))(
              (card: Card) => {
                State.modify[CardHands](
                  (addCard(Discard, card, _: CardHands)): CardHands => CardHands
                )
              }
            )
            .map(_ => None)
        }
      }

      case (None, None) =>
        State.get.map(_ => None)

      case _ => throw Exception("One player doesn't have enough cards for war.")

    }

  def playRound(
    gamestate: GameState[(Option[Card], Option[Card])]
  ): GameState[Option[RoundWinner]] =
    State.apply(s => {
      playBattle(gamestate).run(s).value match {
        case (s2, battleWinner @ Some(_)) => (s2, battleWinner)
        case (s2, None) => playWar(State.get.flatMap(_ => drawCardsForWar)).run(s2).value
      }
    })

  def playGame(
    gamestate: GameState[(Option[Card], Option[Card])]
  ): GameState[Option[RoundWinner]] =
    State.apply(s => {
      Iterator
        .unfold(s)(s2 => {
          val (s3, a) = playRound(gamestate).run(s2).value
          Some(((s3, a), s3))
        })
        .find { case (cardHands, _) =>
          Seq(PlayerOne, PlayerTwo).map(k => cardHands.get(k).toSeq.flatten.size).exists(_ == 0)
        }
        .get
    })

}
