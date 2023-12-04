import scala.util.matching.Regex
import scala.io.Source.fromFile
import scala.math.{max, min}

object DayThree:

  case class Range (from: Int, until: Int):
    def contains (x: Int): Boolean =
      x <= until && x >= from

  case class Point (x: Int, y: Int)

  case class Box (xRange: Range, yRange: Range):
    def contains (point: Point): Boolean =
      xRange.contains (point.x) && yRange.contains (point.y)

  val symbolMatch = "[^0-9.]".r
  val numberMatch = "[0-9]+".r

  case class NumberResult (matched: Int, bounds: Box)
  case class SymbolResult (matched: String, point: Point)

  def parseNumbers (index: Int, maxIndex: Int, line: String, regex: Regex): Iterator [NumberResult] =
    for
      result <- regex.findAllMatchIn (line)
    yield
      val xRange = Range (max (result.start - 1, 0), min (result.end, maxIndex))
      NumberResult (result.matched.toInt, Box (xRange, Range (max (0, index - 1), min (maxIndex, index + 1))))


  def parseSymbols (index: Int, line: String, regex: Regex): Iterator [SymbolResult] =
    for
      result <- regex.findAllMatchIn (line)
    yield
      SymbolResult (result.matched, Point (result.start, index))

  def connections (symbol: SymbolResult, numbers: List [NumberResult]): List [Int] =
    assert (symbol.matched == "*")
    numbers.foldLeft (List.empty [Int]): (agg, rhs) =>
      if rhs.bounds.contains (symbol.point) then
        agg.appended (rhs.matched)
      else
        agg



object DayThreePartOne:

  def solution () =
    val data = fromFile ("data/daythree.txt")
    val lines = data.getLines.toArray
    val maxIndex = lines.indices.last

    val symbols = lines.indices.map { i => DayThree.parseSymbols (i, lines (i), DayThree.symbolMatch).toSeq }
    val numbers = lines.indices.map { i => DayThree.parseNumbers (i, maxIndex, lines (i), DayThree.numberMatch).toSeq }

    val flattened = symbols.flatten

    val partNumbers =
      for
        numberLine <- numbers
        result <- numberLine.filter { nl => flattened.exists (s => nl.bounds.contains (s.point))}
      yield
        result
    val result = partNumbers.map (_.matched).sum
    println (result)

    data.close ()

object DayThreePartTwo:
  @main
  def solution () =
    val data = fromFile ("data/daythree.txt")
    val lines = data.getLines.toArray
    val maxIndex = lines.indices.last

    val symbols = lines.indices.map { i => DayThree.parseSymbols (i, lines (i), DayThree.symbolMatch).toSeq }
    val numbers = lines.indices.map { i => DayThree.parseNumbers (i, maxIndex, lines (i), DayThree.numberMatch).toSeq }

    val flattened = symbols.flatten.filter (_.matched == "*")
    val flattenedNum = numbers.flatten.toList

    val gearCandidates =
      for
        symbol <- flattened
      yield
        DayThree.connections (symbol, flattenedNum)

    val result = gearCandidates.filter (_.length == 2).map (_.product).sum
    println (result)
    data.close ()




