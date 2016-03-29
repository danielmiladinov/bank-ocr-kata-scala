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
  
  private sealed trait ParseState
  private case object Valid extends ParseState
  private case object Illegible extends ParseState

  def parse (representation: IndexedSeq[String]): String = {
    val glyphsByRow = representation.map(_.grouped(3).toIndexedSeq)
    val glyphsByCol = (glyphsByRow(0), glyphsByRow(1), glyphsByRow(2)).zipped
    val digits = glyphsByCol.map {
      case (top, mid, bot) => toDigit.get(List(top, mid, bot).mkString("\n"))
    }

    val parsed = digits.foldLeft(("", Valid): (String, ParseState))((a, d) => {
      val (ds, ps) = a
      d match {
        case Some(x) => (ds + x, ps)
        case None => (ds + "?", Illegible)
      }
    })

    parsed match {
      case (n, Valid) => n
      case (n, Illegible) => s"$n ILL"
    }
  }
}
