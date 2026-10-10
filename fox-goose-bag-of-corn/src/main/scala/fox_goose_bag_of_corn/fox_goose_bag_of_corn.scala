package fox_goose_bag_of_corn

import RiverCrossingEntity.*

enum RiverCrossingEntity:
  case Fox, Goose, Corn, You, Boat

val 🦊 = Fox
val 🪿 = Goose
val 🌽 = Corn
val 🤷 = You
val ⛵  = Boat

enum RiverPosition:
  case LeftBank, Boat, RightBank

enum YouPosition:
  case LeftBank, RightBank, BoatFromLeft, BoatFromRight

type RiverCrossingSection = Set[RiverCrossingEntity]

type RiverCrossingStep = Seq[RiverCrossingSection]
