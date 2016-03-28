package net.miladinov.bankOcr

object Generator {
  def main (args: Array[String]): Unit = {
    val (digits, glyphs) = (1 to 500).foldLeft((List[String](), List[String]()))((acc, _) => {
      val (ds, gs) = acc
      val (digits, glyphs) = randomAccountNumber()
      (digits :: ds, glyphs :: gs)
    })
    
    digits.foreach(println)
    
    println("\n\n-------------------------------------------\n\n")
    
    glyphs.foreach(println)
  }
  
  private val rng = new scala.util.Random()

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
  
  private val toGlyph = toDigit.map(_.swap)

  private def randomAccountNumber(): (String, String) = {
    val digits: IndexedSeq[String] = IndexedSeq.fill(9)(rng.nextInt(10).toString)
    
    val glyphs = digits.map(toGlyph(_).split("\n"))
      .foldLeft(IndexedSeq("", "", "", ""))((acc, char) => {
        val IndexedSeq(top, mid, bot, blank) = acc
        val Array(a, b, c) = char
        IndexedSeq(top + a, mid + b, bot + c, blank)
      })
    
    (digits.mkString, glyphs.mkString("\n"))
  }
}
