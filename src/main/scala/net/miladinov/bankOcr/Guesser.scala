package net.miladinov.bankOcr

object Guesser {

  def additions (g: String): Set[String] = additions(g.split("\n"))

  def additions (glyph: IndexedSeq[String]): Set[String] = {
    val transforms: List[IndexedSeq[String] => IndexedSeq[String]] = List(
      changeTop("_"),
      changeMidLeft("|"),
      changeMidMid("_"),
      changeMidRight("|"),
      changeBotLeft("|"),
      changeBotMid("_"),
      changeBotRight("|")
    )

    transforms
      .map(x => (glyph, x(glyph)))
      .filterNot { case (a, b) => a == b }
      .map { case (a, b) => b.mkString("\n") }
      .toSet
  }

  def removals (g: String): Set[String] = removals(g.split("\n"))

  def removals (glyph: IndexedSeq[String]): Set[String] = {
    val transforms: List[IndexedSeq[String] => IndexedSeq[String]] = List(
      changeTop(" "),
      changeMidLeft(" "),
      changeMidMid(" "),
      changeMidRight(" "),
      changeBotLeft(" "),
      changeBotMid(" "),
      changeBotRight(" ")
    )

    transforms
      .map(x => (glyph, x(glyph)))
      .filterNot { case (a, b) => a == b }
      .map { case (a, b) => b.mkString("\n") }
      .toSet
  }

  private def changeTop (c: String): (IndexedSeq[String]) => IndexedSeq[String] = g => {
    val IndexedSeq(top, mid, bot) = g
    val Array(l, _, r) = top.split("")
    IndexedSeq(IndexedSeq(l, c, r).mkString, mid, bot)
  }

  private def changeMidLeft (c: String): (IndexedSeq[String]) => IndexedSeq[String] = g => {
    val IndexedSeq(top, mid, bot) = g
    val Array(_, m, r) = mid.split("")
    IndexedSeq(top, IndexedSeq(c, m, r).mkString, bot)
  }

  private def changeMidMid (c: String): (IndexedSeq[String]) => IndexedSeq[String] = g => {
    val IndexedSeq(top, mid, bot) = g
    val Array(l, _, r) = mid.split("")
    IndexedSeq(top, IndexedSeq(l, c, r).mkString, bot)
  }

  private def changeMidRight (c: String): (IndexedSeq[String]) => IndexedSeq[String] = g => {
    val IndexedSeq(top, mid, bot) = g
    val Array(l, m, _) = mid.split("")
    IndexedSeq(top, IndexedSeq(l, m, c).mkString, bot)
  }

  private def changeBotLeft (c: String): (IndexedSeq[String]) => IndexedSeq[String] = g => {
    val IndexedSeq(top, mid, bot) = g
    val Array(_, m, r) = bot.split("")
    IndexedSeq(top, mid, IndexedSeq(c, m, r).mkString)
  }

  private def changeBotMid (c: String): (IndexedSeq[String]) => IndexedSeq[String] = g => {
    val IndexedSeq(top, mid, bot) = g
    val Array(l, _, r) = bot.split("")
    IndexedSeq(top, mid, IndexedSeq(l, c, r).mkString)
  }

  private def changeBotRight (c: String): (IndexedSeq[String]) => IndexedSeq[String] = g => {
    val IndexedSeq(top, mid, bot) = g
    val Array(l, m, _) = bot.split("")
    IndexedSeq(top, mid, IndexedSeq(l, m, c).mkString)
  }
}
