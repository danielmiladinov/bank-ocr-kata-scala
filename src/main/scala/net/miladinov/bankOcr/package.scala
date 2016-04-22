package net.miladinov

package object bankOcr {
  type Glyph = IndexedSeq[String]

  val toDigit: Map[String, String] = Map(
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

  val toGlyph = toDigit.map(_.swap)
}
