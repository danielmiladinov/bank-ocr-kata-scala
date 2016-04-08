package net.miladinov.bankOcr

object Parser {
  private val toDigit: Map[String, String] = Map(
    """ _ 
      || |
      ||_|""".stripMargin -> "0",

    """   
      |  |
      |  |""".stripMargin -> "1",

    """ _ 
      | _|
      ||_ """.stripMargin -> "2",

    """ _ 
      | _|
      | _|""".stripMargin -> "3",

    """   
      ||_|
      |  |""".stripMargin -> "4",

    """ _ 
      ||_ 
      | _|""".stripMargin -> "5",

    """ _ 
      ||_ 
      ||_|""".stripMargin -> "6",

    """ _ 
      |  |
      |  |""".stripMargin -> "7",

    """ _ 
      ||_|
      ||_|""".stripMargin -> "8",

    """ _ 
      ||_|
      | _|""".stripMargin -> "9"
  )

  sealed trait ParseState
  case object Valid extends ParseState
  case object Illegible extends ParseState

  def parseDigit (representation: IndexedSeq[String]): (String, ParseState) = {
    toDigit get representation.mkString("\n") match {
      case Some(x) => (x, Valid)
      case None => ("?", Illegible)
    }
  }

  def linesToGlyphs (lines: IndexedSeq[String]): IndexedSeq[IndexedSeq[String]] = {
    // Crib off of what Tuple3Zipped does for its map, because I want to return a real Tuple3, 
    // and there's no implicit conversion from Tuple3Zipped[A] => (A, A, A) and I don't feel like writing one
    import scala.collection.mutable
    val glyphsByRow = lines.map(_.grouped(3).toIndexedSeq)
    val gs = new mutable.ArrayBuffer[IndexedSeq[String]]()
    val IndexedSeq(top, mid, bot) = glyphsByRow
    val m = mid.iterator
    val b = bot.iterator
    for (t <- top) {
      if (m.hasNext && b.hasNext) {
        gs.append(IndexedSeq(t, m.next, b.next))
      }
    }
    gs.toIndexedSeq
  }

  def parse (representation: IndexedSeq[String]): String = {
    val glyphsByCol = linesToGlyphs(representation)
    val digits = glyphsByCol.map(parseDigit)

    convert(digits) match {
      case (n, Valid) => if (Validator.validate(n)) n else s"$n ERR"
      case (n, Illegible) => s"$n ILL"
    }
  }

  def convert (digits: IndexedSeq[(String, ParseState)]): (String, ParseState) =
    digits.foldLeft(("", Valid): (String, ParseState))((a, d) => {
      val (parsedString, stringParseState) = a
      val (parsedDigit, digitParseState) = d
      (parsedString + parsedDigit, if (stringParseState == Valid) digitParseState else stringParseState)
    })
}
