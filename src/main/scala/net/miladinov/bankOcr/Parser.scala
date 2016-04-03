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

  def parse (representation: IndexedSeq[String]): String = {
    val glyphsByRow = representation.map(_.grouped(3).toIndexedSeq)
    val glyphsByCol = (glyphsByRow(0), glyphsByRow(1), glyphsByRow(2)).zipped
    val digits = glyphsByCol.map {
      case (top, mid, bot) => parseDigit(IndexedSeq(top, mid, bot))
    }

    val parsed = digits.foldLeft(("", Valid): (String, ParseState))((a, d) => {
      val (parsedString, stringParseState) = a
      val (parsedDigit, digitParseState) = d
      (parsedString + parsedDigit, if (stringParseState == Valid) digitParseState else stringParseState)
    })

    parsed match {
      case (n, Valid) => if (Validator.validate(n)) n else s"$n ERR"
      case (n, Illegible) => s"$n ILL"
    }
  }
}
