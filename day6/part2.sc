object Day6 {
  def part2(input: String): Long = {
    val rows = input.split("\n").toList
    val cols = rows.map(_.split("").toList).transpose

    def splitByPredicate[A](lists: List[List[A]], isDelimiter: List[A] => Boolean): List[List[List[A]]] = {
      lists.dropWhile(isDelimiter) match {
        case Nil => Nil
        case remaining =>
          val (group, rest) = remaining.span(!isDelimiter(_))
          group :: splitByPredicate(rest, isDelimiter)
      }
    }
    case class Op(operator: String, operands: List[Long])

    val groups = splitByPredicate[String](cols, _.forall(_.isBlank))
    val ops = groups.map { group =>
      val operator = group.head.last
      val operands = group.map { num =>
        num.filter(_.toLongOption.nonEmpty).mkString("").toLong
      }
      Op(operator, operands)
    }
    ops.map { case Op(operator, operands) =>
      operator match {
        case "*" => operands.product
        case "+" => operands.sum
        case _ => 0L
      }
    }.sum
  }
}