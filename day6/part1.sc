object Day6 {
  def part1(input: String): Long = {
    val rows = input.split("\n").toList
    val cols = rows.map(_.split("\\s+").toList).transpose

    cols.map { col =>
      val operands = col.init.map(_.toLong)
      val operator = col.last
      operator match {
        case "*" => operands.product
        case "+" => operands.sum
        case _ => 0L
      }
    }.sum
  }
}

