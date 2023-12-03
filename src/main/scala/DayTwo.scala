import scala.io.Source.fromFile
import scala.math.max

object DayTwo:

  trait Colour:
    def quantity: Int

  case class Red (quantity: Int = 0) extends Colour
  case class Green (quantity: Int = 0) extends Colour
  case class Blue (quantity: Int = 0) extends Colour

  case class Contents (reds: Red = Red (), greens: Green = Green (), blues: Blue = Blue ()):
    def contains (rhs: Contents): Boolean =
      reds.quantity >= rhs.reds.quantity &&
        greens.quantity >= rhs.greens.quantity &&
        blues.quantity >= rhs.blues.quantity

    def containsAll (rhs: Seq [Contents]): Boolean =
      rhs.forall (contains)

    def minBound (contents: Seq [Contents]): Contents =
      contents.foldLeft (Contents ()): (agg, rhs) =>
        Contents.apply (
          max (agg.reds.quantity, rhs.reds.quantity),
          max (agg.greens.quantity, rhs.greens.quantity),
          max (agg.blues.quantity, rhs.blues.quantity)
        )
    def power: Int =
      reds.quantity * greens.quantity * blues.quantity

  object Contents:
    def apply (red: Int, green: Int, blue: Int): Contents =
      Contents (Red (red), Green (green), Blue (blue))

  def mapper (quantity: Int, colour: String): Colour =
    colour match
      case "red" => Red (quantity)
      case "blue" => Blue (quantity)
      case "green" => Green (quantity)

  def handful (colours: Seq [Colour]): Contents =
    colours.foldLeft (Contents ()): (agg, rhs) =>
      rhs match
        case Red (qty) => Contents (Red (qty + agg.reds.quantity), agg.greens, agg.blues)
        case Green (qty) => Contents (agg.reds, Green (qty + agg.greens.quantity), agg.blues)
        case Blue (qty) => Contents (agg.reds, agg.greens, Blue (qty + agg.blues.quantity))

  val container = Contents (12, 13, 14)

  def parse (line: String): (Int, Seq [Contents]) =
    val Array (idStr, data) = line.split (':').map (_.trim)
    val id = idStr.split (' ').map (_.trim).last.toInt
    val contents = data.split (";").map: hf =>
      val colours = hf.split (",").map { c =>
        val Array (qty, colour) = c.strip.split (' ').map (_.trim)
        mapper (qty.toInt, colour) }
      handful (colours.toSeq)
    (id, contents)


object DayTwoPartOne:

  def solve () =
    val data = fromFile ("./data/daytwo.txt")

    val samples =
      for
        line <- data.getLines
      yield
        DayTwo.parse (line)
    val result =
      samples
        .map { case (id, contents) => (id, DayTwo.container.containsAll (contents)) }
        .filter (_._2).map (_._1).sum

    println (result)
    data.close ()

object DayTwoPartTwo:
  @main
  def solve () =
    val data = fromFile ("./data/daytwo.txt")

    val samples =
      for
        line <- data.getLines
      yield
        DayTwo.parse (line)
    val result =
      samples
        .map { case (id, contents) => DayTwo.container.minBound (contents).power }
        .sum

    println (result)
    data.close ()

