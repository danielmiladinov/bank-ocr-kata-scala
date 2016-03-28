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
  
  def parse (representation: IndexedSeq[String]): String = {
    val glyphsByRow = representation.map(_.grouped(3).toIndexedSeq)
    val glyphsByCol = (glyphsByRow(0), glyphsByRow(1), glyphsByRow(2)).zipped
    val digits = glyphsByCol.map {
      case (top, mid, bot) => toDigit(List(top, mid, bot).mkString("\n"))
    }
    digits.mkString
  }
}
