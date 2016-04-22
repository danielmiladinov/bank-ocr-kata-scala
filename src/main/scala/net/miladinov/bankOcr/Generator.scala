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



  private def randomAccountNumber(): (String, String) = {
    var valid = false
    var digits: IndexedSeq[String] = IndexedSeq()
    
    while (!valid) {
      digits = IndexedSeq.fill(9)(rng.nextInt(10).toString)
      valid = Validator.validate(digits)
    }
    
    (digits.mkString, mkGlyph(digits))
  }

  def mkGlyph (number: IndexedSeq[String]): String = {
    number.map(toGlyph(_).split("\n")).foldLeft(IndexedSeq("", "", "", ""))((acc, char) => {
      val IndexedSeq(top, mid, bot, blank) = acc
      val Array(a, b, c) = char
      IndexedSeq(top + a, mid + b, bot + c, blank)
    }).mkString("\n")
  }

  def mkGlyph (number: String): String = mkGlyph(number.split(""))
}
