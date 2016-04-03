package net.miladinov.bankOcr

object Guesser {
  def corrections (g: String): Set[IndexedSeq[String]] = corrections(g.split("\n"))

  def corrections (glyph: IndexedSeq[String]): Set[IndexedSeq[String]] = {
    additions(glyph) union removals(glyph) filter(g => Parser.parseDigit(g)._2 == Parser.Valid)
  }

  def additions (g: String): Set[IndexedSeq[String]] = additions(g.split("\n"))

  def additions (glyph: IndexedSeq[String]): Set[IndexedSeq[String]] = {
    performTransformations(List(
      changeTop("_"),
      changeMidLeft("|"),
      changeMidMid("_"),
      changeMidRight("|"),
      changeBotLeft("|"),
      changeBotMid("_"),
      changeBotRight("|")
    ), glyph)
  }

  def removals (g: String): Set[IndexedSeq[String]] = removals(g.split("\n"))

  def removals (glyph: IndexedSeq[String]): Set[IndexedSeq[String]] = {
    performTransformations(List(
      changeTop(" "),
      changeMidLeft(" "),
      changeMidMid(" "),
      changeMidRight(" "),
      changeBotLeft(" "),
      changeBotMid(" "),
      changeBotRight(" ")
    ), glyph)
  }

  type StringTransformer = IndexedSeq[String] => IndexedSeq[String]

  private def performTransformations (transforms: List[StringTransformer], glyph: IndexedSeq[String]): Set[IndexedSeq[String]] = {
    transforms
      .map(x => (glyph, x(glyph)))
      .filterNot { case (a, b) => a == b }
      .map { case (a, b) => b }
      .toSet
  }

  private def changeTop (c: String): StringTransformer = g => {
    val IndexedSeq(top, mid, bot) = g
    val Array(l, _, r) = top.split("")
    IndexedSeq(IndexedSeq(l, c, r).mkString, mid, bot)
  }

  private def changeMidLeft (c: String): StringTransformer = g => {
    val IndexedSeq(top, mid, bot) = g
    val Array(_, m, r) = mid.split("")
    IndexedSeq(top, IndexedSeq(c, m, r).mkString, bot)
  }

  private def changeMidMid (c: String): StringTransformer = g => {
    val IndexedSeq(top, mid, bot) = g
    val Array(l, _, r) = mid.split("")
    IndexedSeq(top, IndexedSeq(l, c, r).mkString, bot)
  }

  private def changeMidRight (c: String): StringTransformer = g => {
    val IndexedSeq(top, mid, bot) = g
    val Array(l, m, _) = mid.split("")
    IndexedSeq(top, IndexedSeq(l, m, c).mkString, bot)
  }

  private def changeBotLeft (c: String): StringTransformer = g => {
    val IndexedSeq(top, mid, bot) = g
    val Array(_, m, r) = bot.split("")
    IndexedSeq(top, mid, IndexedSeq(c, m, r).mkString)
  }

  private def changeBotMid (c: String): StringTransformer = g => {
    val IndexedSeq(top, mid, bot) = g
    val Array(l, _, r) = bot.split("")
    IndexedSeq(top, mid, IndexedSeq(l, c, r).mkString)
  }

  private def changeBotRight (c: String): StringTransformer = g => {
    val IndexedSeq(top, mid, bot) = g
    val Array(l, m, _) = bot.split("")
    IndexedSeq(top, mid, IndexedSeq(l, m, c).mkString)
  }
}
