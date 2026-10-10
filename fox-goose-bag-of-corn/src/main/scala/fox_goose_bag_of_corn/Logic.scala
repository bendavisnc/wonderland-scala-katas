package fox_goose_bag_of_corn

import YouPosition.*

object Logic {

  def findWhereAreYou(steps: Seq[RiverCrossingStep]): YouPosition =
    val mostRecent: RiverCrossingStep = steps.last
    mostRecent match {
      case Seq(leftBank, _, _) if leftBank.contains(🤷)   => LeftBank
      case Seq(_, _, rightBank) if rightBank.contains(🤷) => RightBank
      case Seq(_, boat, _) if boat.contains(🤷)           => {
        val secondMostRecent: RiverCrossingStep = steps.apply(steps.length - 2)
        secondMostRecent match {
          case Seq(leftBank, _, _) if leftBank.contains(🤷)   => BoatFromLeft
          case Seq(_, _, rightBank) if rightBank.contains(🤷) => BoatFromRight
          case _ => throw Exception("Unexpected `You` location (after seen in boat).")
        }
      }
      case _ => throw Exception("Unexpected `You` location.")
    }

  def isBoatCapacityRespected(step: RiverCrossingStep): Boolean =
    step(RiverPosition.Boat.ordinal).toSeq match {
      case Seq(_)       => true // no ones in the boat
      case Seq(_, _)    => true // boat + 1
      case Seq(_, _, _) => true // boat + carrier + carriee
      case _            => false
    }

  def isEveryoneSafe(step: RiverCrossingStep): Boolean =
    step.forall {
      case riverStation if riverStation.contains(🤷)          => true
      case riverStation if Set(🪿, 🌽).subsetOf(riverStation) => false
      case riverStation if Set(🦊, 🪿).subsetOf(riverStation) => false
      case _                                                  => true
    }

  def everyoneMadeIt(step: RiverCrossingStep): Boolean =
    Set(🦊, 🪿, 🌽, 🤷) == step(RiverPosition.RightBank.ordinal)

}
