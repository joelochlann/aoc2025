object Day7 {
  def part2(input: String): Long = {
    case class Beam(location: Int, possiblePaths: Long)
    case class State(beams: Set[Beam])
    val firstLine :: lines = input.split("\n").toList
    val initialState = State(Set(Beam(firstLine.indexOf("S"), 1L)))
    val finalState = lines.foldLeft(initialState) { (state, line) =>
      val newBeamsList = state.beams.toList.flatMap { beam =>
        if (line(beam.location) == '^') {
          List(Beam(beam.location - 1, beam.possiblePaths), Beam(beam.location + 1, beam.possiblePaths))
        } else {
          List(beam)
        }
      }
      val newBeams = newBeamsList
        .groupBy(_.location)
        .map { case (location, beams) => Beam(location, beams.map(_.possiblePaths).sum) }
        .toSet
      State(newBeams)
    }
    finalState.beams.toList.map(_.possiblePaths).sum
  }
}