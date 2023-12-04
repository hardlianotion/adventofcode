import scala.io.Source.fromFile
import scala.math.pow
import scala.util.matching.Regex


object DayFour:

  val cardGameRegex =
    """(?:Card(?: )+[0-9]{1,3}):((?: ){1,2}(?:[0-9]{1,2}(?: ){1,2}){10})\|((?:(?: ){1,2}[0-9]{1,2}){25})""".r

  val idCardGameRegex =
    """(?:Card(?: )+)([0-9]{1,3}):((?: ){1,2}(?:[0-9]{1,2}(?: ){1,2}){10})\|((?:(?: ){1,2}[0-9]{1,2}){25})""".r

  case class CardGame (winners: IndexedSeq [Int], attempts: IndexedSeq [Int]):
    def count: Int =
      attempts.count { x => winners.contains (x) }

    def value: Int =
      pow (2, count - 1).intValue

  case class IdCardGame (id: Int, game: DayFour.CardGame)

  object CardGame:

    def fromString (line: String, regex: Regex): CardGame =
      val matches = regex.findAllIn (line).subgroups
      val winners = matches.head

      val attempts = matches.apply (1)

      def numbers (line: String): IndexedSeq [Int] =
        line.trim.split ("( )+").map (_.trim.toInt)

      CardGame (numbers (winners), numbers (attempts))

  object IdCardGame:

    def fromString (line: String, regex: Regex): IdCardGame =
      val matches = regex.findAllIn (line).subgroups
      assert (matches.length == 3, s"length = ${matches.length}")

      val id = matches.head.trim.toInt
      val winners = matches.apply (1)
      val attempts = matches.apply (2)

      def numbers (line: String): IndexedSeq [Int] =
        line.trim.split ("( )+").map (_.trim.toInt)

      IdCardGame (id, CardGame (numbers (winners), numbers (attempts)))

    def value (cards: IndexedSeq [IdCardGame]): Int =
      def incrementAt (values: IndexedSeq [Int], range: Range): IndexedSeq [Int] =
        val increment = values (range.head)
        val incRange = range.head + 1 to range.last
        values.indices.map { idx => if incRange.contains (idx) then values (idx) + increment else values (idx) }

      def impl (idx: Int, cards: IndexedSeq [IdCardGame], counts: IndexedSeq [Int], weights: IndexedSeq [Int]): IndexedSeq [Int] =
        if idx == cards.length then
          weights
        else
          counts (idx) match
            case 0 => impl (idx + 1, cards, counts, weights)
            case n => impl (idx + 1, cards, counts, incrementAt (weights, idx to idx + n))

      val counts = cards.map (_.game.count)
      impl (0, cards, counts, IndexedSeq.fill (cards.length) (1)).sum


object DayFourPartOne:

  def solution () =
    val data = fromFile ("data/dayfour.txt")

    val result =
      data.getLines.map { l => DayFour.CardGame.fromString (l, DayFour.cardGameRegex).value }.sum

    println (result)

    data.close ()

object DayFourPartTwo:
  @main
  def solution () =
    val data = fromFile ("data/dayfour.txt")

    val result =
      data.getLines.toIndexedSeq.map { l => DayFour.IdCardGame.fromString (l, DayFour.idCardGameRegex) }

    println (DayFour.IdCardGame.value (result))

    data.close ()