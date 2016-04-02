package net.miladinov.bankOcr

object Guesser {

  def additions (g: String): Set[String] = additions(g.split("\n"))

  def additions (glyph: IndexedSeq[String]): Set[String] = {
    val transforms: List[IndexedSeq[String] => IndexedSeq[String]] = List(
      addTop,
      addMidLeft,
      addMidMid,
      addMidRight,
      addBotLeft,
      addBotMid,
      addBotRight
    )

    transforms
      .map(x => (glyph, x(glyph)))
      .filterNot { case (a, b) => a == b }
      .map { case (a, b) => b.mkString("\n") }
      .toSet
  }

  private def addTop (g: IndexedSeq[String]): IndexedSeq[String] = {
    val IndexedSeq(top, mid, bot) = g
    val Array(l, _, r) = top.split("")
    IndexedSeq(IndexedSeq(l, "_", r).mkString, mid, bot)
  }

  private def addMidLeft (g: IndexedSeq[String]): IndexedSeq[String] = {
    val IndexedSeq(top, mid, bot) = g
    val Array(_, m, r) = mid.split("")
    IndexedSeq(top, IndexedSeq("|", m, r).mkString, bot)
  }

  private def addMidMid (g: IndexedSeq[String]): IndexedSeq[String] = {
    val IndexedSeq(top, mid, bot) = g
    val Array(l, _, r) = mid.split("")
    IndexedSeq(top, IndexedSeq(l, "_", r).mkString, bot)
  }

  private def addMidRight (g: IndexedSeq[String]): IndexedSeq[String] = {
    val IndexedSeq(top, mid, bot) = g
    val Array(l, m, _) = mid.split("")
    IndexedSeq(top, IndexedSeq(l, m, "|").mkString, bot)
  }

  private def addBotLeft (g: IndexedSeq[String]): IndexedSeq[String] = {
    val IndexedSeq(top, mid, bot) = g
    val Array(_, m, r) = bot.split("")
    IndexedSeq(top, mid, IndexedSeq("|", m, r).mkString)
  }

  private def addBotMid (g: IndexedSeq[String]): IndexedSeq[String] = {
    val IndexedSeq(top, mid, bot) = g
    val Array(l, _, r) = bot.split("")
    IndexedSeq(top, mid, IndexedSeq(l, "_", r).mkString)
  }

  private def addBotRight (g: IndexedSeq[String]): IndexedSeq[String] = {
    val IndexedSeq(top, mid, bot) = g
    val Array(l, m, _) = bot.split("")
    IndexedSeq(top, mid, IndexedSeq(l, m, "|").mkString)
  }

  def removals (g: String): Set[String] = removals(g.split("\n"))

  def removals (glyph: IndexedSeq[String]): Set[String] = {
    val transforms: List[IndexedSeq[String] => IndexedSeq[String]] = List(
      removeTop,
      removeMidLeft,
      removeMidMid,
      removeMidRight,
      removeBotLeft,
      removeBotMid,
      removeBotRight
    )

    transforms
      .map(x => (glyph, x(glyph)))
      .filterNot { case (a, b) => a == b }
      .map { case (a, b) => b.mkString("\n") }
      .toSet
  }

  private def removeTop (g: IndexedSeq[String]): IndexedSeq[String] = {
    val IndexedSeq(top, mid, bot) = g
    val Array(l, _, r) = top.split("")
    IndexedSeq(IndexedSeq(l, " ", r).mkString, mid, bot)
  }

  private def removeMidLeft (g: IndexedSeq[String]): IndexedSeq[String] = {
    val IndexedSeq(top, mid, bot) = g
    val Array(_, m, r) = mid.split("")
    IndexedSeq(top, IndexedSeq(" ", m, r).mkString, bot)
  }

  private def removeMidMid (g: IndexedSeq[String]): IndexedSeq[String] = {
    val IndexedSeq(top, mid, bot) = g
    val Array(l, _, r) = mid.split("")
    IndexedSeq(top, IndexedSeq(l, " ", r).mkString, bot)
  }

  private def removeMidRight (g: IndexedSeq[String]): IndexedSeq[String] = {
    val IndexedSeq(top, mid, bot) = g
    val Array(l, m, _) = mid.split("")
    IndexedSeq(top, IndexedSeq(l, m, " ").mkString, bot)
  }

  private def removeBotLeft (g: IndexedSeq[String]): IndexedSeq[String] = {
    val IndexedSeq(top, mid, bot) = g
    val Array(_, m, r) = bot.split("")
    IndexedSeq(top, mid, IndexedSeq(" ", m, r).mkString)
  }

  private def removeBotMid (g: IndexedSeq[String]): IndexedSeq[String] = {
    val IndexedSeq(top, mid, bot) = g
    val Array(l, _, r) = bot.split("")
    IndexedSeq(top, mid, IndexedSeq(l, " ", r).mkString)
  }

  private def removeBotRight (g: IndexedSeq[String]): IndexedSeq[String] = {
    val IndexedSeq(top, mid, bot) = g
    val Array(l, m, _) = bot.split("")
    IndexedSeq(top, mid, IndexedSeq(l, m, " ").mkString)
  }
}
