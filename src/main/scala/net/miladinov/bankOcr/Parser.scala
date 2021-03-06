package net.miladinov.bankOcr

object Parser {
  sealed trait ParseState
  case object Valid extends ParseState
  case object Illegible extends ParseState

  def parseDigit (glyph: Glyph): (String, ParseState) = {
    toDigit get glyph.mkString("\n") match {
      case Some(x) => (x, Valid)
      case None => ("?", Illegible)
    }
  }

  def linesToGlyphs (lines: IndexedSeq[String]): IndexedSeq[Glyph] = {
    // Crib off of what Tuple3Zipped does for its map, because I want to return a real Tuple3, 
    // and there's no implicit conversion from Tuple3Zipped[A] => (A, A, A) and I don't feel like writing one
    import scala.collection.mutable
    val glyphsByRow = lines.map(_.grouped(3).toIndexedSeq)
    val gs = new mutable.ArrayBuffer[Glyph]()
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
      case (n, Valid) => if (Validator.validate(n)) n else correct(glyphsByCol, n, "ERR")
      case (n, Illegible) => correct(glyphsByCol, n, "ILL")
    }
  }

  def correct (glyphs: IndexedSeq[Glyph], digits: String, failureCode: String): String = {
    val validCorrections = collection.immutable.SortedSet[String]() ++ Guesser.combineCorrections(glyphs)
      .map(gs => convert(gs.map(parseDigit)))
      .filter {
        case (n, Valid) => Validator.validate(n)
        case _ => false
      }
      .map(_._1)

    if (validCorrections.isEmpty)
      s"$digits $failureCode"
    else if (validCorrections.size == 1)
      validCorrections.head
    else
      s"$digits AMB ${validCorrections.map(t => s"'$t'").mkString("[", ", ", "]")}"
  }

  def convert (digits: IndexedSeq[(String, ParseState)]): (String, ParseState) =
    digits.foldLeft(("", Valid): (String, ParseState))((a, d) => {
      val (parsedString, stringParseState) = a
      val (parsedDigit, digitParseState) = d
      (parsedString + parsedDigit, if (stringParseState == Valid) digitParseState else stringParseState)
    })
}
