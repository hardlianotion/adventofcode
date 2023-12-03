import scala.io.Source.fromFile
import scala.util.matching.Regex
import scala.util.{Success, Failure, Try}

object DayOnePartOne:
  val matcher: Regex = "[0-9]".r

  def solution (): Unit =
    val data = fromFile ("./data/dayone.txt")

    val digits =
      for
        line <- data.getLines
      yield
        val matches = matcher.findAllIn (line).toSeq

        10 * matches.head.toInt + matches.last.toInt


    println (digits.sum)
    data.close ()

object DayOnePartTwo:
  val matcher: Regex = "(zero|one|two|three|four|five|six|seven|eight|nine|[0-9])".r
  val reverseMatcher: Regex = "(orez|eno|owt|eerht|ruof|evif|xis|neves|thgie|enin|[0-9])".r

  val mapper =
    Map ("zero" -> 0, "one" -> 1, "two" -> 2, "three" -> 3, "four" -> 4,
      "five" -> 5, "six" -> 6, "seven" -> 7, "eight" -> 8, "nine" -> 9)

  val reverseMapper =
    Map ("orez" -> 0, "eno" -> 1, "owt" -> 2, "eerht" -> 3, "ruof" -> 4,
      "evif" -> 5, "xis" -> 6, "neves" -> 7, "thgie" -> 8, "enin" -> 9)

  @main
  def solution (): Unit =
    val data = fromFile ("./data/dayone.txt")

    val digits =
      for
        line <- data.getLines
      yield
        val enil = line.reverse
        val matched = matcher.findAllIn (line).toSeq.head
        val reverseMatched = reverseMatcher.findAllIn (enil).toSeq.head

        def attempt (item: String, converter: Map [String, Int]): Int =
          Try {
            item.toInt
          } match
            case Success (n) => n
            case Failure (_) => converter (item)

        //println (s"$matched - $reverseMatched")
        10 * attempt (matched, mapper) + attempt (reverseMatched, reverseMapper)

    println (digits.sum)
    data.close ()






