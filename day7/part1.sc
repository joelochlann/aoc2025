object Day7 {
  def part1(input: String): Int = {
    case class State(beamLocations: Set[Int], splits: Int)
    val firstLine :: lines = input.split("\n").toList
    val initialState = State(Set(firstLine.indexOf("S")), 0)
    val finalState = lines.foldLeft(initialState) { (state, line) =>
      var splits = state.splits
      val newBeamLocations = state.beamLocations.flatMap { beamLocation =>
        if (line(beamLocation) == '^') {
          splits += 1
          Set(beamLocation - 1, beamLocation + 1)
        } else {
          Set(beamLocation)
        }
      }
      State(newBeamLocations, splits)
    }
    finalState.splits
  }
}